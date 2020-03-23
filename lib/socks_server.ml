let handle_irc_client (target:Socks.socks4_request) proxy_details certs
    first_data (client_in,client_out) =
  Logs.debug (fun m -> m "%s: connecting to %a" __LOC__
                 Fmt.(pair string int) (target.address, target.port)) ;
  Logs.err (fun m -> m "TODO prepend onto input from app: %S" first_data);
  assert (first_data = "") ;
  connect_proxy proxy_details certs >>= function
  | Error err ->
    Logs.err (fun m -> m "err: %a" Rresult.R.pp_msg err) ; Lwt.return_unit
  | Ok (proxy_in, proxy_out) ->
    Logs.debug (fun m -> m "connected to proxy") ;

    (* We are connected to the proxy, ask for current status to identify
       the case where we need to reconnect to an existing connection: *)
    Lwt_io.write proxy_out (serialize_status 0l Int32.max_int) >>= fun () ->
    Lwt_io.flush proxy_out >>= fun () ->

    let string_of_state = function
      | `Initial_status_answer -> "Initial_status_answer"
      | `Handle_connect_response -> "Handle_connect_response"
      | `Established -> "Established"
    and fatal msg = return (`Fatal msg)
    in

    let rec loop ~state needed_len acc =
      Logs.debug (fun m -> m "entering loop - %d" needed_len) ;
      (* Here is where messages coming in from proxy are read *)
      Lwt_io.read ~count:needed_len proxy_in >>= fun msg ->
      if "" = msg then fatal "handle_irc_client->loop: connection closed TODO"
      else
        let msg = String.concat "" [acc ; msg] in
        let decoded_msg = unserialized_of_server_msg msg in
        Logs.debug (fun m -> m "%a" pp_server_message decoded_msg);
        begin match state , decoded_msg with
          | _ , `Need_more count ->
            loop ~state count msg

          | _ , `Invalid _ ->
            fatal @@ "Failed to decode received data in state "
                     ^ (string_of_state state)

          | (`Initial_status_answer, (`Connect_answer _
                                     |`Incoming _|`Outgoing_ACK _))
          | (`Established, `Connect_answer _)
          | (`Handle_connect_response, (`Incoming _
                                       |`Outgoing_ACK _|`Status_answer _))
            ->  (* Handle invalid combinations of (state , received msg): *)
            fatal @@ "invalid (parseable) msg received in state "
                     ^ (string_of_state state) ^ "received: " ^ msg

          | `Initial_status_answer , `Status_answer (ans:status_answer list) ->
            Lwt_list.iter_s
              (function ({conn_id ; ping_interval ; address ; port;
                         seq_num ; count_queued}:status_answer) ->
                 if (target:Socks.socks4_request).address = address
                 && target.port = port
                 then begin
                   Logs.warn (fun m ->
                       m "FOUND EXISTING CONNECTION FOR SAME HOST \
                          conn_id: %ld ping int: %d address: %s port: %d \
                          seq_num: %Ld queued pings: %ld"
                         conn_id ping_interval address port seq_num
                         count_queued) ;
                   Lwt.return_unit
                     (* need to decrypt or supply tls engine state:
                        Hashtbl.replace states { tls_state = Tls.Engine.state ;
                        outgoing = [] ;
                        address ; port ; max_covered_sequence } *)
                 end else begin
                   Logs.warn (fun m ->
                       m "existing connection: %ld ping interval: %d \
                          address: %s port: %d seq_num: %Ld \
                          count_queued: %ld"
                         conn_id ping_interval address port seq_num
                         count_queued) ;
                   Lwt.return_unit
                 end
              )
              ans >>= fun () ->
            (*TODO: only if we don't have an existing state: *)
            (* Ask the proxy to connect to target.address: *)
            let target : Socks.socks4_request = target in
            begin match Tlsping.serialize_connect 20
                          (target.address, target.port) with
              | None ->
                Logs.err (fun m ->
                    m "error: unable to serialize connect to \
                       '%s':%d" target.address target.port) ;
                Lwt.return_unit
              | Some connect_msg ->
                Lwt_io.write proxy_out connect_msg
            end
            >>= fun () -> return `Handle_connect_response

          (* Handle response to our CONNECT message: *)
          | `Handle_connect_response , `Connect_answer (conn_id , _ , _)
            when conn_id = 0l -> (* if failed *)
            fatal @@ Printf.sprintf "error from proxy: no connect to \
                                     %s" target.address
          (*TODO kill connection*)
          | `Handle_connect_response , `Connect_answer (conn_id , _ , _) ->
            Logs.debug (fun m -> m "=> Connect_answer conn_id %ld" conn_id) ;
            (* Initialize TLS connection from client -> target *)
            let authenticator =
              x509_fingerprint_authenticator_ignoring_time target.address
               target.username in
            return @@ Tls.Config.client
              ~peer_name:target.address (* TODO not ideal for e.g. onions?
                                           this is currently here so that we can
                                           reconstruct the Authenticator in
                                           Tlsping.deserialize_tls_state
                                        *)
              ~authenticator
              ~ciphers:[ `TLS_DHE_RSA_WITH_AES_256_CBC_SHA256
                       ; `TLS_DHE_RSA_WITH_AES_256_CBC_SHA ]
              ~version:Tls.Core.(TLS_1_2 , TLS_1_2) (* g *)
              ~hashes:[`SHA256 ; `SHA1] ()
            >>= fun tls_config ->
            return @@ Tls.Engine.client tls_config
            >>= fun (tls_state, client_hello) ->
            (* Initiate a TLS connection: *)
            Hashtbl.add states conn_id {tls_state; outgoing = [] ;
                                        address = target.address ;
                                        port = target.port ;
                                        unencrypted_outgoing = [] ;
                                        max_covered_sequence = 0L ;
                                        (* replay_queue = [] *)
                                        } ;
            Lwt_io.write proxy_out (serialize_outgoing conn_id 0L
                                      Cstruct.(to_string client_hello)
                                   ) >>= fun () ->
            Lwt_io.flush proxy_out >>= fun () ->
            (* setup thread that encrypts outgoing messages by proxy *)
            Lwt.async (handle_outgoing conn_id client_in proxy_out) ;
            return @@ `Established

          | `Established , `Incoming (conn_id , next_seq , queued_seq , msg) ->
            handle_incoming ~proxy_out ~client_out ~conn_id ~next_seq
              ~queued_seq ~msg

          | `Established , `Outgoing_ACK (conn_id ,
                                          `Ok ,
                                          acked_seq ,
                                          _next_seq) ->
            let conn_state = Hashtbl.find states conn_id in (*TODO handle not found?*)
            (* message was sent; remove it from local buffer *)
            let outgoing =
              let rec f acc = function
                | (s , _ ) :: tl when -1 = Int64.compare s acked_seq ->
                  f acc tl
                | hd :: tl -> f (hd::acc) tl
                | [] -> List.rev acc
              in f [] conn_state.outgoing
            in
            conn_state.outgoing <- outgoing ;
            Hashtbl.replace states conn_id conn_state ;
            Logs.warn (fun m -> m "outgoing ack: TODO cleanup buffers") ;
            return `Established

          | `Established , `Outgoing_ACK (conn_id ,
                                          `Resend ,
                                          acked_seq ,
                                          next_seq) ->
            handle_resend_ack ~proxy_out ~conn_id ~acked_seq ~next_seq

          | `Established , `Status_answer _lst ->
            Logs.debug (fun m -> m "%a"
                           Fmt.(styled `Cyan @@ styled_unit `Underline "Status_answer") ()) ;
            return `Established

    end (* begin match state_machine *)
    >>= function
    | (`Established | `Handle_connect_response) as new_state ->
      loop ~state:(new_state) 2 ""
    | fatal -> return fatal
  in
  begin loop ~state:`Initial_status_answer 2 "" >>= function
  | `Fatal msg ->
      Logs.err (fun m -> m "%s" msg) ; Lwt.return_unit
  | _ ->
      failwith "TODO unhandled exit state\n"
  end

(*
let handle_client (unix_fd, sockaddr) proxy certs () =
  begin match sockaddr with
  | Lwt_unix.ADDR_INET ( _ (*inet_addr*), port) ->
    Logs.debug (fun m -> m "Incoming connection, src port: %d" port) ;
    Lwt.return_unit
  | Lwt_unix.ADDR_UNIX _ -> return ()
  end >>= fun () ->
  let client_in  = Lwt_io.of_fd ~mode:Input  unix_fd
  and client_out = Lwt_io.of_fd ~mode:Output unix_fd in
  Socks.receive_request client_in >>= function
  | `Invalid_fingerprint fp ->
    Logs.err (fun m -> m "socks4 handler: invalid sha256 fingerprint: %s"
                 fp) ; Lwt.return_unit (* TODO cancel this *)
  | `Invalid_request ->
    Logs.err (fun m -> m "invalid request!") ;
    Lwt.fail (Invalid_argument "invalid request!") (*TODO failwith / logging *)
  | `Socks4 ({Socks.port ; username; address } as target) ->
    Logs.debug (fun m -> m "got request for host '%s' port %d fp %s"
      address port username) ;
    handle_irc_client client_in client_out target proxy certs
*)

let socks_request_policy server_fingerprint
    (proxy_details, certs)
  : 'a Socks_lwt.request_policy =
  fun req ->
    let open Lwt_result in
  begin match req with
  | `Socks4 req -> Lwt_result.return req
  | `Socks5 Connect { address = Domain_address address ; port } ->
    Lwt_result.return ({address ; port;
                        username = server_fingerprint }:Socks.socks4_request)
  (* FIXME, if connect is to ip address, hostname is hardcoded *)
  | `Socks5 Connect { address = IPv4_address address ; port } ->
    Lwt_result.return ({address = (Ipaddr.V4.to_string address) ; port;
                        username = server_fingerprint }:Socks.socks4_request)
  | `Socks5 Connect { address = IPv6_address _; _ } -> Lwt_result.fail (`Msg "socks5 ipv6 not supported")
  | `Socks5 Bind _ -> Lwt_result.fail (`Msg "socks5 bind not supported")
  | `Socks5 UDP_associate _ -> Lwt_result.fail (`Msg "socks5 UDP_associate not supported")
  end >>= fun (target : Socks.socks4_request) ->
  (* TODO make socks able to let us specify a custom setup function *)
  let cb : Socks_lwt.client_data_cb =
    (fun data channel ->
       handle_irc_client target proxy_details certs data channel)
  in
  Lwt_result.return cb

let socks5_auth_policy proxy_and_certs : ('a,'b) Socks_lwt.auth_policy =
  let validate_fp username =
    (* tlsping uses the "user_id" field in socks4 to hold the hex-encoded
       sha256 fingerprint of the x509 certificate of address:port
    *)
    if 64 <> String.length username
    then Error (`Invalid_fingerprint username)
    else try Ok (Hex.of_string username) with _ ->
      Error (`Invalid_fingerprint username)
  in
  fun lst ->
  if List.exists (function Socks.Username_password _ -> true
                         | _ -> false) lst
  then Lwt_result.return
      (Socks.Username_password ("",""),
       (function
         | Socks.Username_password (username, _) ->
           begin match validate_fp username with
             | Ok fp ->
               Lwt_result.return (socks_request_policy (Hex.to_string fp) proxy_and_certs)
             | _ -> Lwt_result.fail (`Msg "")
           end
         | _ -> Lwt_result.fail (`Msg "TODO")
       ))
  else Lwt_result.fail (`Msg "Not using username-password auth")

let socks_address_policy proxy_and_certs : ('a,'b,'c) Socks_lwt.address_policy =
  Socks_lwt.client_allow_localhost (socks5_auth_policy proxy_and_certs)

let listener_service (host,port) proxy_and_certs =
  let open Lwt_unix in
  create_socket host >>= function
  | Error () -> failwith "unable to resolve listen addr"
  | Ok (s , host_inet_addr) ->
  Lwt_unix.close s >>= fun () ->
  (*   let () = listen s 10 in TODO *)
  (*(Lwt.async (handle_client c proxy certs) ;*)
  (* server is the socks server *)
  let server = Socks_lwt.establish_server (ADDR_INET (host_inet_addr, port ))
      (socks_address_policy proxy_and_certs) in
  server



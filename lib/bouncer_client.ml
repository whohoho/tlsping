

let states = Hashtbl.create 5

let connect_proxy (host , port) certs
  : (Lwt_io.input_channel * Lwt_io.output_channel, [> R.msg])Lwt_result.t =
  Tlsping.(proxy_tls_config certs) >>=
  function { authenticator ; ciphers ; version ; hashes ; certificates } ->
    let config = Tls.Config.client ~authenticator ~ciphers ~version
        ~hashes ~certificates () in
    Logs.debug (fun m -> m "connecting to proxy") ;
    Lwt.catch (fun () ->
        let open Lwt_result in
        create_socket host
        |> Lwt_result.map_err (function
            | () -> `Msg "unable to resolve proxy hostname" )
        >>= fun (fd, proxy) ->
        let open Lwt_result in
        (* the ServerName on the cert might not match the actual hostname
           we use, so we need to provide it in ~host: below *)
        ok @@ Lwt_unix.connect fd (ADDR_INET (proxy, port)) >>= fun() ->
        Logs.warn (fun m -> m "TODO expected hostname of tlsping server is \
                               still hardcoded to 'proxy.example.org'");
        (ok @@ Tls_lwt.Unix.client_of_fd config ~host:"proxy.example.org" fd)
        >|= fun tls_t -> Tls_lwt.(of_t tls_t))
      (function
        | Unix.Unix_error (Unix.ECONNREFUSED, f_n , _) ->
          Lwt_result.lift
            (R.error_msgf "Unix error: connection refused: %S" f_n)
        | Tls_lwt.Tls_failure err ->
          Lwt_result.lift @@ R.error_msgf
            "Tls_failure: %s"
            begin match err with
              | `Error (`AuthenticationFailure (valerr)) ->
                Fmt.strf "ServerName mismatch: %a"
                  X509.Validation.pp_validation_error valerr
              | _ -> "XXYYZ" ^ Tls.Engine.string_of_failure err
            end
        | _ -> failwith "TODO catch this exception"
      )




let handle_outgoing conn_id client_in proxy_out () =
  (* Read from client, send to upstream *)
  let rec loop () =
    let conn_state = Hashtbl.find states conn_id in (*TODO handle not found*)
    (* TODO handle disconnect / broken line: *)
    (Lwt_io.read_line client_in >|= fun line ->
    (* when using Lwt_io.read instead:
       if "" = line then raise End_of_file ; *)
    let line = line ^ "\r\n" in
    conn_state.unencrypted_outgoing <-
      conn_state.unencrypted_outgoing @ [line] ) >>= fun () ->
    let rec wait_for_encryption first =
      match conn_state.tls_state.encryptor with
      | None ->
        if first then
          Logs.warn (fun m -> m "outgoing: queuing since no encryptor state") ;
        Lwt_unix.sleep 0.5 >>= fun () -> wait_for_encryption false
      | Some _ -> Lwt.return ()
    in wait_for_encryption true >>= fun () ->
    let target_sequence = match conn_state.tls_state.encryptor with
      | None -> failwith "cannot happen?" | Some {sequence; _} -> sequence in
    begin match encrypt_queue conn_state.tls_state
                  (conn_state.unencrypted_outgoing)
                  target_sequence with
    | Ok ( tls_state , msg_list ) ->
      conn_state.unencrypted_outgoing <- [] ;
      conn_state.tls_state <- tls_state ;
      let _ = checkpoint_states () in
      let serialized_msgs =
        List.map (fun (msg_sequence, cout, `Plaintext plaintext) ->
            conn_state.outgoing <-
              (msg_sequence , plaintext) :: conn_state.outgoing ;
            serialize_outgoing conn_id msg_sequence Cstruct.(to_string cout)
          ) msg_list |> String.concat ""
      in
      (*TODO: if this fails, wait for reconnect: *)
      Lwt_io.write proxy_out serialized_msgs >>= fun() ->
      send_pings_if_needed conn_id proxy_out
    | Error _ ->
      Logs.err (fun m -> m "Unable to encrypt and send outgoing message");
      Lwt.return_unit
    end
    >>= fun () -> loop ()
  in
  Lwt.catch loop
    (function End_of_file -> Logs.err (fun m -> m "%s: EOF" __LOC__); return ()
            | e -> raise e)

let handle_incoming ~proxy_out ~client_out ~conn_id ~next_seq ~queued_seq ~msg =
  (* proxy_out is the connection tls_ping_server;
     client_out is the connection to the IRC client *)
  (* TODO decrypt ; handle write errors; buffer if client disconnected? *)
  let conn_state = Hashtbl.find states conn_id in (*TODO handle not found?*)
  begin match Tls.Engine.handle_tls conn_state.tls_state
                Cstruct.(of_string msg) with
  | `Ok (`Ok tls_state , `Response resp, `Data msg) ->
    begin match tls_state.encryptor with
      | Some {cipher_st ; sequence } ->
        conn_state.tls_state
        <- {tls_state with
            encryptor = Some
                ({cipher_st ;
                  sequence = Tlsping.int64_max sequence
                      (if next_seq <> Int64.max_int
                       then next_seq else 0L)
                 }:Tls.State.crypto_context)}
      | None ->
        conn_state.tls_state <- tls_state
    end ;
    conn_state.max_covered_sequence <- int64_max queued_seq
        conn_state.max_covered_sequence ;
    let _ = checkpoint_states () in
    begin match resp with
      | Some resp_data ->
        let sequence = begin match tls_state.encryptor with
          | Some crypto_context -> crypto_context.sequence
          | None-> failwith "TODO no encryption context in tls_state" end in
        Logs.debug (fun m -> m "Upstream: need to transmit") ;
        Lwt_io.write proxy_out (serialize_outgoing conn_id sequence
                                  Cstruct.(to_string resp_data))
        >>= fun () -> Lwt_io.flush proxy_out
      | None -> return ()
    end
    >>= fun() ->
    begin match msg with
      | Some msg_data ->
        (* TODO
           String.index_from ":server.example.org PONG server.example.org :TLSPiNG:10" 0 ' '
           let first_space = 1 + String.index_from msg_data 0 ' ' in
           let second_space = String.index_from msg_data first_space ' ' in
           let server = String.sub msg_data 1 (first_space -2) in
           match String.sub msg_data first_space (second_space - first_space) with
           | "PONG :TLSPiNG:"
        *)
        Logs.debug (fun m ->
            m "Incoming: remote next_seq: %Ld queued_seq: %Ld"
              next_seq queued_seq) ;
        Lwt_io.write client_out Cstruct.(to_string msg_data)
        >>= fun () -> send_pings_if_needed conn_id proxy_out
      | None ->
        Logs.debug (fun m -> m "Upstream: INCOMING: NO MSGDATA");
        Lwt.return_unit
    end
    >>= fun () -> return @@ `Established
  | `Ok (`Alert typ, `Response resp , `Data msg) ->
    let _ = resp , msg in (*TODO*)
    Lwt.return (`Fatal (Fmt.strf "Upstream TLS ALERT %s"
                        @@ Tls.Packet.alert_type_to_string typ))
  | `Ok (`Eof , `Response resp , `Data msg) ->
    let _ = resp , msg in (*TODO*)
    Lwt.return (`Fatal "Upstream TLS EOF")
  | `Fail (failure , `Response resp) ->
    let _ = resp in (*TODO*)
    Lwt.return @@ `Fatal (Printf.sprintf "Upstream TLS FAIL: %s"
                          @@ Tls.Engine.string_of_failure failure)
  end



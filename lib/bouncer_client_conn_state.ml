 open Rresult 

open Lwt
(* open Lwt.Infix *)
(* open Shared *)


(*
open Lwt.Infix
*)
type encrypt_msg_error =
| TLS_handshake_not_finished
| TLS_state_error


type serialized_state = string

let proxy_out = stdout
(*
type state_get = unit -> unit Lwt.t

type state_put =  serialized_state -> (string, error) result Lwt.t
*
*)
module Remote = struct
type t = {
  ; address           : string
  ; port              : int
  ; hostname          : string
}
end

used_multiplex_ids = Set.

module BouncerIO = struct
  (* queue for bouncerio of a single connection, the data channel *) 

  type t = {
    ; queue_to_bouncer : string Ke.Fke.t
    ; queue_from_bouncer : string  Ke.Fke.t
  }

  let send_bouncer_data m = Ke.Fke.push t.queue_to_bouncer m 
  let read_bouncer_data = Ke.Fke.pop t.queue_to_bouncer

end


module ClientIO struct

   type state_get = unit -> (serialized_state, Mirage_kv.error) Stdlib.result Lwt.t
   type state_put =  serialized_state -> (unit, Mirage_kv.write_error) Stdlib.result Lwt.t

  type t = {
    ; channel_to_client : string Stream.t
    ; channel_from_client : string Stream.t
    ; state_store_get : state_get
    ; state_store_put : state_put
  }

  let send_bouncer_control = Ke.Fke.push t.queue_to_bouncer m 
  let read_bouncer_control m = Ke.Fke.pop t.queue_to_bouncer
  let write_client m =
  let read_client m =  

end

(* the state of 1 tls session via a bouncer *)
type t =
  { conn_id : Int32.t
  ; target : Remote.t
  ; mutable tls_state : Tls.Engine.state
  ; mutable outgoing  : (int64 * string) list
  ; mutable unencrypted_outgoing : string list
  ; mutable max_covered_sequence : int64
  ; client_in
  ; proxy_out 
  ; outgoing
  ; io : IO.t
  }

(* ask the proxy to setup a connection to remote, takes a Target.t returns a IO.t*)
let setup_bouncer_data_channel remote io = 
    (* Ask the proxy to connect to target.address: *)
    (* give it a multiplex id  TODO: extend the connect message to include the multiplex id*)
    let id = 1234 in

    

    begin match Shared.serialize_connect 20 (remote.address, remote.port) with
      | None ->
        Logs.err (fun m ->
            m "error: unable to serialize connect to \
               '%s':%d" remote.address remote.port) ;
        Lwt.return_unit
      | Some connect_msg ->
        (* send the connect message on bouncer control channel *)
        send_bouncer_control connect_msg
    end

(* setup the tls session via proxy to remote *) 
let setup_tls_to_remote io remote =
  let authenticator =
              x509_fingerprint_authenticator_ignoring_time target.address
               target.username in
    Lwt.return @@ Tls.Config.client
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
       (* [client client] is [tls * out] where [tls] is the initial state,
          and [out] the initial client hello  *)
    Lwt return @@ Tls.Engine.client tls_config
        >>= fun (tls_state, client_hello) ->
          (* Initiate a TLS connection: *)


    (* save the stuff in type t and return it *)
    let newt {  tls_state
              ; outgoing = [] ;
              ; target = target;
              ; io = io
              ; unencrypted_outgoing = [] 
              ;  max_covered_sequence = 0L } in
(* send the client_hello to proxy *)
  Lwt_io.write proxy_out (serialize_outgoing t.conn_id 0L
                            Cstruct.(to_string client_hello)
                         ) >>= fun () ->
  Lwt_io.flush proxy_out >>= fun () ->
  (* setup thread that encrypts outgoing messages by proxy *)
  Lwt.async (handle_outgoing t.conn_id t.client_in t.proxy_out) ;
  return @@ `Established

let remove_acked_message ~t =
    (* message was sent; remove it from local buffer *)
    let outgoing =
      let rec f acc = function
        | (s , _ ) :: tl when -1 = Int64.compare s acked_seq ->
          f acc tl
        | hd :: tl -> f (hd::acc) tl
        | [] -> List.rev acc
      in f [] t.outgoing
    in
    t.outgoing <- outgoing ;
    Logs.warn (fun m -> m "outgoing ack: TODO cleanup buffers") ;
    return `Established


(*   --  *)


let to_string t =
  "bouncer_client_conn_state with: " ^ t.address 

let checkpoint_state ~t =
  Logs.info (fun m -> m "TODO checkpointing TLS state of: ");
   (* TODO deserialize and return new state after serializing to disk,
     that way we can do some casual testing that this is actually resumable...*)
   (* FIXME check for error *)
   let _ = t.state_store_put (Shared.serialize_tls_state t.tls_state) in
   t.state_store_get () >|= function
   | Ok s -> Shared.deserialize_tls_state s
   |  _ -> print_string "state_get failed!!!"; t.tls_state

let handle_outgoing conn_id client_in proxy_out () ~t =
  (* Read from client, send to bouncer *)
  let rec loop () =
    (* TODO handle disconnect / broken line: *)
    (Lwt_io.read_line client_in >|= fun line ->
    (* when using Lwt_io.read instead:
       if "" = line then raise End_of_file ; *)
    let line = line ^ "\r\n" in
    t.unencrypted_outgoing <-
      t.unencrypted_outgoing @ [line] ) >>= fun () ->
    let rec wait_for_encryption first =
      match t.tls_state.encryptor with
      | None ->
        if first then
          Logs.warn (fun m -> m "outgoing: queuing since no encryptor state") ;
        Lwt_unix.sleep 0.5 >>= fun () -> wait_for_encryption false
      | Some _ -> Lwt.return ()
    in wait_for_encryption true >>= fun () ->
    let target_sequence = match t.tls_state.encryptor with
      | None -> failwith "cannot happen?" | Some {sequence; _} -> sequence in
    begin match encrypt_queue t.tls_state
                  (t.unencrypted_outgoing)
                  target_sequence with
    | Ok ( tls_state , msg_list ) ->
      t.unencrypted_outgoing <- [] ;
      t.tls_state <- tls_state ;
      let _ = checkpoint_states () in
      let serialized_msgs =
        List.map (fun (msg_sequence, cout, `Plaintext plaintext) ->
            t.outgoing <-
              (msg_sequence , plaintext) :: conn_state.outgoing ;
            serialize_outgoing t.conn_id msg_sequence Cstruct.(to_string cout)
          ) msg_list |> String.concat ""
      in
      (*TODO: if this fails, wait for reconnect: *)
      Lwt_io.write t.proxy_out serialized_msgs >>= fun() ->
      send_pings_if_needed ~t
    | Error _ ->
      Logs.err (fun m -> m "Unable to encrypt and send outgoing message");
      Lwt.return_unit
    end
    >>= fun () -> loop ()
  in
  Lwt.catch loop
    (function End_of_file -> Logs.err (fun m -> m "%s: EOF" __LOC__); return ()
            | e -> raise e)

let handle_incoming ~next_seq ~queued_seq ~msg ~t =
  (* proxy_out is the connection tls_ping_server;
     client_out is the connection to the IRC client *)
  (* TODO decrypt ; handle write errors; buffer if client disconnected? *)
  begin match Tls.Engine.handle_tls t.tls_state
                Cstruct.(of_string msg) with
  | `Ok (`Ok tls_state , `Response resp, `Data msg) ->
    begin match t.tls_state.encryptor with
      | Some {cipher_st ; sequence } ->
        t.tls_state
        <- {tls_state with
            encryptor = Some
                ({cipher_st ;
                  sequence = Tlsping.int64_max sequence
                      (if next_seq <> Int64.max_int
                       then next_seq else 0L)
                 }:Tls.State.crypto_context)}
      | None ->
        t.tls_state <- tls_state
    end ;
    t.max_covered_sequence <- int64_max queued_seq
        t.max_covered_sequence ;
    let _ = checkpoint_states () in
    begin match resp with
      | Some resp_data ->
        let sequence = begin match t.encryptor with
          | Some crypto_context -> crypto_context.sequence
          | None-> failwith "TODO no encryption context in tls_state" end in
        Logs.debug (fun m -> m "Upstream: need to transmit") ;
        Lwt_io.write t.proxy_out (serialize_outgoing t.conn_id sequence
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
        >>= fun () -> send_pings_if_needed ~t
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



let encrypt_queue tls_state payloads seq_num_offset =
  let rec encrypt_msg tls_state payloads acc =
    (* encrypt a record containing [payload] and MAC'd with the given [seq_num],
     * using the client keys from [tls_state] *)
    begin match payloads , tls_state.Tls.State.encryptor with
      | (payload :: payloads) , Some encryptor ->
        begin match Tls.Engine.send_application_data
                      tls_state [Cstruct.(of_string payload)] with
        | None -> R.error TLS_state_error
        | Some (tls_state , encrypted) ->
          encrypt_msg tls_state payloads
            ((encryptor.sequence , encrypted, `Plaintext payload) :: acc)
        end
      | [] , Some _ -> R.ok (tls_state , List.rev acc)
      | _  , None   -> R.error TLS_state_error
    end
   in 
  match tls_state.Tls.State.encryptor with
  | Some { sequence ; _ } when (sequence > seq_num_offset) ->
    (* the `when` guard makes sure we only encrypt data "ahead" of time *)
    R.error TLS_state_error (* TODO error type for this *)
  | Some { cipher_st ; _ } ->
    let new_state : Tls.State.state =
      {tls_state with encryptor =
                        (Some {Tls.State.cipher_st ;
                               sequence = seq_num_offset }
                         :Tls.State.crypto_state)} in
    encrypt_msg new_state payloads []
  | None ->
    R.error TLS_state_error

let send_pings_if_needed ~t =
  let get_seq s = begin match s.Tls.State.encryptor with
    | Some {sequence; _} -> sequence | None -> -1337L end in
  let offset     = Shared.int64_max t.max_covered_sequence (get_seq t.tls_state) in
  let next_seq   = Int64.succ @@ get_seq t.tls_state in
  let new_offset =
    let rec lol proposed =
      begin match 1 = Int64.compare offset proposed with
        | false -> begin match 1 = Int64.compare next_seq Int64.(sub proposed 5L) with
            | false -> proposed
            | true -> lol Int64.(add proposed 10L)
          end
        | true  -> lol Int64.(add proposed 10L)
      end in lol 10L
  in
  let pings =
    let rec gen_pings acc = function
      | 0L -> acc
      | i ->
        gen_pings
          (Printf.(sprintf "PING :TLSPiNG:%Ld\r\n" Int64.(add i offset))::acc)
          Int64.(pred i)
    in
    t.max_covered_sequence <- new_offset ;
    gen_pings [] Int64.(sub new_offset offset)
  in
  if pings = [] then begin
    Logs.debug (fun m ->
        m "not sending pings since we are at offset %Ld and have %Ld queued"
          next_seq t.max_covered_sequence) ;
    Lwt.return_unit
  end else begin
    Logs.debug (fun m ->
        m "sending %d pings: amount %Ld max: %Ld offset: %Ld"
          List.(length pings) new_offset t.max_covered_sequence offset
      ) ;
    begin match encrypt_queue t.tls_state pings offset with
      | Ok (_ , pings) ->
        let open Lwt in
        Lwt_list.map_s (fun (seq, cout, `Plaintext _) ->
            Logs.debug (fun m -> m "queuing %Ld" seq) ;
            (return @@ Cstruct.to_string cout)
          ) pings >>= fun pings ->
        Lwt_list.iter_s (fun m ->  let _ = Ke.Fke.push t.queue_to_bouncer m in 
                                   Lwt.return_unit)
        (* FIXME: Shared.serialize_queue does something with conn id *)
        @@ Shared.serialize_queue ~conn_id:2323l offset pings >>= fun () ->
        Lwt.return_unit
      | Error _ ->
        Logs.err (fun m -> m "outgoing: TODO error generating PINGs") ;
        Lwt.return_unit
    end
  end

let handle_resend_ack ~acked_seq ~next_seq ~t =
  (* TODO update next_seq *)
  (* FIXME put conid in debug *)
  Logs.debug (fun m -> m "[%ld] %a Got a request to resend seq %Ld, next: %Ld"
                 2323l Fmt.(styled_unit `Underline "RESEND") ()
                 acked_seq next_seq) ;
  begin match t.tls_state.encryptor with
    | Some encryptor ->
      if acked_seq < next_seq && encryptor.sequence <= next_seq then begin
        (*conn_state.tls_state <- {tls_state with
              encryptor = Some {encryptor with sequence = next_seq}} ;*)
        let line =
          let rec rec_l = function
            | [] -> failwith "TODO line to be resent doesn't exist\n"
            | (s, m) :: _ when s = acked_seq -> m
            | _ :: tl -> rec_l tl
          in rec_l t.outgoing
        in
        begin match encrypt_queue t.tls_state [line] next_seq with
          | Error _ -> return @@ `Fatal
              (Printf.sprintf "resend, but error re-encrypting \
                               TODO die ns:%Ld" next_seq)
          | Ok (tls_state , msg_list) ->
            t.tls_state <- tls_state ;
            let msgs = List.map (fun (resent_seq, cout, `Plaintext plaintext) ->
                Logs.debug (fun m -> m "[%ld] Resending seq %Ld: @[<v>%S@]"
                               2323l resent_seq plaintext);
                (* FIXME serialize_outgoing does something with conn_id *)
                Shared.serialize_outgoing 2323l resent_seq
                @@ Cstruct.to_string cout)
                msg_list
            in
            (*TODO reinject into outgoing queue and clear old entry *)
            t.outgoing <- (next_seq ,
                                    line) :: t.outgoing ;
            (* Lwt_io.write stdout String.(concat "" msgs) *)
            (* FIXME write the message to queue *)
            let _ = Ke.Fke.push t.queue_to_bouncer String.(concat "" msgs)  in  
            Lwt.return_unit
            >>= fun () -> send_pings_if_needed ~t
            >>= fun () -> return @@ `Established
        end
      end else
        Lwt.return @@ `Fatal
          (Printf.sprintf
             "TODO was asked to resend, but acked %Ld ; \
              next %Ld ; current encryptor.seq %Ld"
             acked_seq next_seq encryptor.sequence)
    | None ->
      Lwt.return @@ `Fatal "resend, but no connection TODO die"
  end

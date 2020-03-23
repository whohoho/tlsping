

open Rresult
(* open Lwt.Infix *)
open Lwt
(* open Shared *)


(*
open Lwt.Infix
*)
type encrypt_msg_error =
| TLS_handshake_not_finished
| TLS_state_error


(*
 *
type serialized_state = string
type state_get = unit -> (serialized_state, Mirage_kv.error) result Lwt.t
type state_put =  serialized_state -> (unit, Mirage_kv.write_error) result Lwt.t
*)

(* the state of 1 tls session via a bouncer *)
type t =
  { mutable tls_state : Tls.Engine.state
  ; mutable outgoing  : (int64 * string) list
  ; mutable unencrypted_outgoing : string list
  ; address           : string
  ; port              : int
  ; hostname          : string
  ; mutable max_covered_sequence : int64
  ; queue_to_bouncer : string Ke.Fke.t
  ; queue_from_bouncer : string  Ke.Fke.t
  ; channel_to_client : string Stream.t
  ; channel_from_client : string Stream.t
  (*
  ; state_store_get : state_get
  ; state_store_put : state_put
  *)
  }

let to_string t =
  "bouncer_client_conn_state with: " ^ t.address 
(*
let checkpoint_state ~t =
  Logs.info (fun m -> m "TODO checkpointing TLS state of: ");
   (* TODO deserialize and return new state after serializing to disk,
     that way we can do some casual testing that this is actually resumable...*)
   (* FIXME check for error *)
   let _ = t.state_store_put (Shared.serialize_tls_state t.tls_state) in
   t.state_store_get () >|= function
   | Ok s -> Shared.deserialize_tls_state s
   |  _ -> print_string "state_get failed!!!"; t.tls_state
*)

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

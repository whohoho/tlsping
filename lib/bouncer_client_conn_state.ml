type encrypt_msg_error =
| TLS_handshake_not_finished
| TLS_state_error



type state_get = string -> (string, error) result Lwt.t
type state_put = string -> (unit, write_error) result Lwt.t 


(* the state of 1 tls session via a bouncer *)
type t =
  { mutable tls_state : Tls.Engine.state
  ; mutable outgoing  : (int64 * string) list
  ; mutable unencrypted_outgoing : string list
  ; address           : string
  ; port              : int
  ; hostname          : string
  ; mutable max_covered_sequence : int64
  ; queue_to_bouncer : Ke.Rke.t
  ; queue_from_bouncer : Ke.Rke.t
  ; channel_to_client : Stream.t
  ; channel_from_client : Stream.t
  ; state_store_get : state_get
  ; state_store_put : state_put
  }

let to_string t =
  "bouncer_client_conn_state with: " ^ t.address 

let checkpoint_state ~t =
  Logs.info (fun m -> m "TODO checkpointing TLS state of: " ^ (to_string t));
   (* TODO deserialize and return new state after serializing to disk,
     that way we can do some casual testing that this is actually resumable...*)
   (* FIXME check for error *)
   let _ = state_store_put (serialize_tls_state t.tls_state) in
   match state_store_get with
   | string s -> deserialize_tls_state s
   | error e -> print_string "state_get failed!!!"; t.tls_state
    



let encrypt_queue () payloads seq_num_offset ~t =
  let tls_state = t.tls_state in
  let rec encrypt_msg t.tls_state payloads acc ~t =
    (* encrypt a record containing [payload] and MAC'd with the given [seq_num],
     * using the client keys from [tls_state] *)
    begin match payloads , t.tls_state.Tls.State.encryptor with
      | (payload :: payloads) , Some encryptor ->
        begin match Tls.Engine.send_application_data
                      t.tls_state [Cstruct.(of_string payload)] with
        | None -> R.error TLS_state_error
        | Some (t.tls_state , encrypted) ->
          encrypt_msg t.tls_state payloads
            ((encryptor.sequence , encrypted, `Plaintext payload) :: acc) ~t
        end
      | [] , Some _ -> R.ok (t.tls_state , List.rev acc)
      | _  , None   -> R.error TLS_state_error
    end
  in
  match t.tls_state.Tls.State.encryptor with
  | Some { sequence ; _ } when (sequence > seq_num_offset) ->
    (* the `when` guard makes sure we only encrypt data "ahead" of time *)
    R.error TLS_state_error (* TODO error type for this *)
  | Some { cipher_st ; _ } ->
    let new_state : Tls.State.state =
      {t.tls_state with encryptor =
                        (Some {Tls.State.cipher_st ;
                               sequence = seq_num_offset }
                         :Tls.State.crypto_state)} in
    encrypt_msg new_state payloads [] ~t
  | None ->
    R.error TLS_state_error

let send_pings_if_needed conn_id proxy_out ~t =
  let get_seq s = begin match s.Tls.State.encryptor with
    | Some {sequence; _} -> sequence | None -> -1337L end 
  in
  (* let conn_state = Hashtbl.find states conn_id in (*TODO handle not found*) *)
  let offset     = int64_max t.max_covered_sequence (get_seq t.tls_state) in
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
    (* what is this ?? Hashtbl.replace states conn_id conn_state ; *)
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
    begin match encrypt_queue _ pings offset ~t with
      | Ok (_ , pings) ->
        Lwt_list.map_s (fun (seq, cout, `Plaintext _) ->
            Logs.debug (fun m -> m "queuing %Ld" seq) ;
            (return @@ Cstruct.to_string cout)
          ) pings >>= fun pings ->
        Lwt_list.iter_s (fun m -> Ke.Rke.push t.queue_to_bouncer m)
        @@ serialize_queue ~conn_id offset pings
      | Error _ ->
        Logs.err (fun m -> m "outgoing: TODO error generating PINGs") ;
        Lwt.return_unit
    end
  end

let handle_resend_ack ~proxy_out ~conn_id ~acked_seq ~next_seq ~t =
  (* TODO update next_seq *)
  Logs.debug (fun m -> m "[%ld] %a Got a request to resend seq %Ld, next: %Ld"
                 conn_id Fmt.(styled_unit `Underline "RESEND") ()
                 acked_seq next_seq) ;
(*  let conn_state = Hashtbl.find states conn_id in (*TODO handle not found?*) *)
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
                               conn_id resent_seq plaintext);
                serialize_outgoing conn_id resent_seq
                @@ Cstruct.to_string cout)
                msg_list
            in
            (*TODO reinject into outgoing queue and clear old entry *)
            t.outgoing <- (next_seq ,
                                    line) :: t.outgoing ;
(*            Hashtbl.replace states conn_id conn_state ; *)
(* FIXME write to queue            Lwt_io.write proxy_out String.(concat "" msgs) *)
            >>= fun () -> send_pings_if_needed conn_id proxy_out
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



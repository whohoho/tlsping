open Lwt
open Ex_common
open Printexc

let print_call_site str =
  (* Iterate over inlined frames *)
  let rec iter_raw_backtrace_slot f slot =
    f slot;
    match get_raw_backtrace_next_slot slot with
    | None -> ()
    | Some slot' -> iter_raw_backtrace_slot f slot' 
  in

  (* Iterate over stack frames *)
  let iter_raw_backtrace f bt =
    for i = 0 to raw_backtrace_length bt - 1 do
      iter_raw_backtrace_slot f (get_raw_backtrace_slot bt i)
    done
  in
  let callstack = get_callstack 30 in
  let parse_loc str = str in 
  let btstring = parse_loc raw_backtrace_to_string callstack in
(*  print_string (btstring ^ "\n"); *)
  iter_raw_backtrace (fun rawslot -> 
    let slot = convert_raw_backtrace_slot rawslot in
    match Printexc.Slot.location slot with
    | None -> ()
    | Some { filename;	line_number; start_char;	end_char;  } -> 
      print_string ( filename ^ ":" 
                     ^ string_of_int line_number ^ " " 
                     ^ string_of_int start_char ^ ":" 
                     ^ string_of_int end_char ^ "\n" )
    ) callstack; 
  print_string "end \n";
  flush stdout;
  ()

let string_of_unix_err err f p =
  Printf.sprintf "Unix_error (%s, %s, %s)"
    (Unix.error_message err) f p

let serve_ssl port callback =

  let tag = "server" in

  X509_lwt.private_of_pems
    ~cert:server_cert
    ~priv_key:server_key >>= fun cert ->

  let server_s () =
    let open Lwt_unix in
    let s = socket PF_INET SOCK_STREAM 0 in
    setsockopt s SO_REUSEADDR true ;
    bind s (ADDR_INET (Unix.inet_addr_any, port)) >|= fun () ->
    listen s 10 ;
    s in

  let handle channels addr =
    async @@ fun () ->
      Lwt.catch (fun () -> callback channels addr >>= fun () -> yap ~tag "<- handler done")
        (function
          | Tls_lwt.Tls_alert a ->
            yap ~tag @@ "handler: " ^ Tls.Packet.alert_type_to_string a
          | Tls_lwt.Tls_failure a ->
            yap ~tag @@ "handler: " ^ Tls.Engine.string_of_failure a
          | Unix.Unix_error (e, f, p) ->
            yap ~tag @@ "handler: " ^ (string_of_unix_err e f p)
          | _exn -> yap ~tag "handler: exception")
  in

  yap ~tag ("-> start @ " ^ string_of_int port) >>= fun () ->
  let rec loop s =
    let authenticator = null_auth in
    let config = Tls.Config.server ~reneg:true ~certificates:(`Single cert) ~authenticator () in
    (Lwt.catch
       (fun () -> Tls_lwt.accept_ext ~trace:eprint_sexp config s >|= fun r -> `R r)
       (function
         | Unix.Unix_error (e, f, p) -> return (`L (string_of_unix_err e f p))
         | Tls_lwt.Tls_alert a -> return (`L (Tls.Packet.alert_type_to_string a))
         | Tls_lwt.Tls_failure f -> return (`L (Tls.Engine.string_of_failure f))
         | _exn -> return (`L "loop: exception"))) >>= function
    | `R (channels, addr) ->
      yap ~tag "-> connect" >>= fun () -> ( handle channels addr ; loop s )
    | `L (msg) ->
      yap ~tag ("server socket: " ^ msg) >>= fun () -> loop s
    in
    server_s () >>= fun s ->
    loop s

let echo_server port =
  serve_ssl port @@ fun (ic, oc) _addr ->
    lines ic |> Lwt_stream.iter_s (fun line ->
      yap ~tag:"handler" ("+ " ^ line) >>= fun () ->
      Lwt_io.write_line oc line)

let () =
  let port =
    try int_of_string Sys.argv.(1) with _ -> 4466
  in
  print_string "testje";
  print_call_site "testje";
  Lwt_main.run (echo_server port)

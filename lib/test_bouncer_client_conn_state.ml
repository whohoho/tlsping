open Alcotest
(* let cs = Bouncer_client_conn_state.t  in *)

open Lwt.Infix
module KV = Mirage_kv_unix
let lwt_run f () = Lwt_main.run (f ())
let do_or_fail = Rresult.R.get_ok

let failf fmt = Fmt.kstrf Alcotest.fail fmt
let assert_fail e = failf "%a" KV.pp_error e
let assert_write_fail e = failf "%a" KV.pp_write_error e
(*
let test_kv () =
  
  let%lwt  kv =  Mirage_kv_unix.connect "/tmp/" in
  let key = Mirage_kv.Key.v "testfile.txt" in
  let set = (Mirage_kv_unix.set kv key) in
  let get = (Mirage_kv_unix.get kv key) in
  let str1 = "Dit is de string" in
  let str2 = 
    Lwt.map (function
      | Ok data -> data
      | Error e -> Mirage_kv.pp_error e)
        get
     in
     set str1
     (check string) "serializing 1" str1 (Lwt_main.run str2);
    Lwt.return_unit
*)

let get_kv dir file =
   Mirage_kv_unix.connect "/tmp/" >>= fun kv ->
  let key = Mirage_kv.Key.v "testfile.txt" in
  let set = (Mirage_kv_unix.set kv key) in
  let get = (Mirage_kv_unix.get kv key) in
  let exists = (Mirage_kv_unix.exists kv key) in
  Lwt.return (set, get, exists) 

let test_kv () =
(*
  Mirage_kv_unix.connect "/tmp/" >>= fun kv ->

  let key = Mirage_kv.Key.v "testfile.txt" in
  let set = (Mirage_kv_unix.set kv key) in
  let get = (Mirage_kv_unix.get kv key) in
   *)
  let%lwt (set, get, exists) = get_kv "/tmp/" "testfile2.txt" in
  let str1 = "Dit is de string" in
  set str1 >|= do_or_fail >>= fun () ->
  exists >>= function
  | Error e ->
    failf "Exists on an existing file failed %a" KV.pp_error e
  | Ok None ->
    failf "Exists on an existing file returned None"
  | Ok (Some `Dictionary) ->
    failf "Exists on an existing file returned a dictionary"
  | Ok (Some `Value) ->
    get >|= do_or_fail >>= fun str2 ->
    Alcotest.(check string) __LOC__ str1 str2;
    Lwt.return_unit



let () =

  lwt_run test_kv ();
  (*
  run "Connstate" [
    "kv", [ 
      "serialization" `Quick lwt_run test_kv;  

          ];
    ]
     *)

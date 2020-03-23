open Printexc

let () =
  Printexc.record_backtrace true

let () = 
print_backtrace stdout

let testprint = 
  print_string "test print! \n";
  print_backtrace stdout; 
  flush stdout

(* Iterate over inlined frames *)
let rec iter_raw_backtrace_slot f slot state  =
  let state = f slot state in
  match get_raw_backtrace_next_slot slot with
  | None -> state ^ ""
  | Some slot' -> iter_raw_backtrace_slot f slot' state

(* Iterate over stack frames *)

let iter_raw_backtrace f bt state =
  for i = 0 to raw_backtrace_length bt - 1 do
    let state = iter_raw_backtrace_slot f (get_raw_backtrace_slot bt i) state in
    print_string ("s: " ^ state);
  done;
  state

let bt_slot_printer rawslot str =
  let slot = convert_raw_backtrace_slot rawslot in
  match Printexc.Slot.location slot with
  | None -> "noo"
  | Some { filename;	line_number; start_char;	end_char;  } ->
    ( filename ^ ":" 
     ^ string_of_int line_number ^ " " 
     ^ string_of_int start_char ^ ":" 
     ^ string_of_int end_char  ^ ">>" ) 

 


let lalabt_slot_printer rawslot state =
  let slot = convert_raw_backtrace_slot rawslot in
  print_string "-- slot printer \n";
  print_backtrace stdout; 
  (Printexc.get_callstack 100) |> (Printexc.print_raw_backtrace stdout);
  print_string "-- end trace \n";
  match Printexc.Slot.location slot with
  | None -> ()
  | Some { filename;	line_number; start_char;	end_char;  } -> 
    print_string ( filename ^ ":" 
                   ^ string_of_int line_number ^ " " 
                   ^ string_of_int start_char ^ ":" 
                   ^ string_of_int end_char  ^ "\n" ) ;
      flush stdout;
  state

let print_trace =
let callstack = get_callstack 100 in
let s = iter_raw_backtrace bt_slot_printer callstack "" in
print_string s;
()

(*
let get_loc =
let callstack = get_callstack 30 in
  let i = ref 0 in
   iter_raw_backtrace (fun rawslot -> 
    let slot = convert_raw_backtrace_slot rawslot in
      print_int i;
      match Printexc.Slot.location slot with
      | None -> ()
      | Some { filename;	line_number; start_char;	end_char;  } -> 
        print_string ( filename ^ ":" 
                       ^ string_of_int line_number ^ " " 
                       ^ string_of_int start_char ^ ":" 
                       ^ string_of_int end_char ^ "\n" )
      ) callstack; 
   

let print_call_site str =
  let callstack = get_callstack 30 in
  let btstring =  raw_backtrace_to_string callstack in
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
*)

let app_style = `Cyan 
let err_style = `Red
let warn_style = `Yellow 
let info_style = `Blue 
let debug_style = `Green 

let my_pp_header ~pp_h ppf (l, h) = 
  print_trace;
  match l with
    | Logs.App ->
        begin match h with
        | None -> ()
        | Some h -> Fmt.pf ppf "[%a] " Fmt.(styled app_style string) h
        end
    | Logs.Error ->
        pp_h ppf err_style (match h with None -> "ERROR" | Some h -> h)
    | Logs.Warning ->
        pp_h ppf warn_style (match h with None -> "WARNING" | Some h -> h)
    | Logs.Info ->
        pp_h ppf info_style (match h with None -> "INFO" | Some h -> h)
    | Logs.Debug ->
        pp_h ppf debug_style (match h with None -> "DEBUG" | Some h -> h)

  let pp_exec_header =
    let x = match Array.length Sys.argv with
    | 0 -> Filename.basename Sys.executable_name
    | n -> Filename.basename Sys.argv.(0)
  in
  let pp_h ppf style h = Fmt.pf ppf "wtf testje! %s: [%a] bt: %s " x Fmt.(styled style string) h 
      (Printexc.raw_backtrace_to_string (Printexc.get_callstack 30)) in
  print_trace;
  my_pp_header ~pp_h 


let _setup_log (style_renderer:Fmt.style_renderer option) level : unit =
  (*
  print_trace;
  testprint;
  (Printexc.get_callstack 100) |> (Printexc.print_raw_backtrace stdout);
  flush stdout;
  *)
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (Logs_fmt.reporter ~pp_header:pp_exec_header ())


type _ state =
  | Expectconnect : [`Expectconnect] state 
  | Connecting : [`Connecting] state
  | Connected : [`Connected] state
  | Disconnecting : [`Disconnecting] state
  | Connecting_subscribed : [`Connecting_subscribed] state
  | Connected_subscribed : [`Connected_subscribed] state

type anystate = Any : 'a state -> anystate

let state_tostring state = 
  let Any state = state in
  match state with
  | Expectconnect -> "Expectconnect"
  | Connecting -> "Connecting"
  | Connected -> "Connected"
  | Disconnecting -> "Disconnecting"
  | Connecting_subscribed -> "Connecting_subscribed"
  | Connected_subscribed -> "Connected_subscribed"



type (_,_) transition =
   | Expectconnect_to_Connecting : ([`Expectconnect], [`Connecting])  transition
(*invalid   | Connecting_to_Disconnecting : ([`Connecting], [`Disconnecting])  transition *)
   | Connecting_to_Connecting_subscribed : ([`Connecting], [`Connecting_subscribed])  transition
(* invalid   | Connecting_subscribed_to_Disconnecting : ([`Connecting_subscribed], [`Disconnecting]) transition *)
   | Connected_to_Connecting : ([`Connected], [`Connecting]) transition
   | Connected_subscribed_to_Connecting_subscribed : ([`Connected_subscribed], [`Connecting_subscribed]) transition
   | Connected_subscribed_to_Connected : ([`Connected_subscribed], [`Connected]) transition
   | Connecting_subscribed_to_Connected_subscribed : ([`Connecting_subscribed], [`Connected_subscribed]) transition
   | Connected_to_Connected_subscribed : ([`Connected], [`Connected_subscribed]) transition
   | Connected_to_Disconnecting : ([`Connected], [`Disconnecting]) transition
   | Connected_subscribed_to_Disconnecting : ([`Connected_subscribed], [`Disconnecting]) transition
   | Connecting_subscribed_to_Connecting : ([`Connecting_subscribed], [`Connecting]) transition

(*   | Subscribe : ([`Connected], [`Connected_subscribed])  transition *)

let transition_msg msg = 
  let s = print_string ("\n>> transition: " ^ msg ^ "\n") in
  flush stdout

let next_state 
      (type current) (type next)
      (transition_witness :((current, next) transition))
      (t:current state) : next state 
        = 
  match transition_witness, t with 
    Connecting_subscribed_to_Connected_subscribed, Connecting_subscribed -> 
      transition_msg "successfull connection (Connected_subscribed)"; Connected_subscribed
  | Connected_to_Connected_subscribed, Connected -> 
      transition_msg "subscribed (connected_subscribed)"; Connected_subscribed
  | Connected_to_Disconnecting, Connected -> 
      transition_msg "disconnecting, waiting for queue to be emty (Disconnecting)"; Disconnecting
  | Connecting_to_Connecting_subscribed, Connecting -> 
      transition_msg "subscribed (Connecting_subscribed)"; Connecting_subscribed
  | Expectconnect_to_Connecting, Expectconnect -> 
      transition_msg "got connect message  (Connecting)"; Connecting
  | Connected_to_Connecting, Connected ->
      transition_msg "reconnecting (Connecting)"; Connecting
  | Connected_subscribed_to_Connecting_subscribed, Connected_subscribed ->
      transition_msg "reconnecting (Connecting_subscribed)"; Connecting_subscribed
  | Connected_subscribed_to_Connected, Connected_subscribed ->
      transition_msg "unsubscribing (Connected)"; Connected
  | Connecting_subscribed_to_Connecting, Connecting_subscribed ->
      transition_msg "unsubscribing (Connecting)"; Connecting
  | Connected_subscribed_to_Disconnecting, Connected_subscribed ->
      transition_msg "disconnecting (Disconnecting)"; Disconnecting



(* not valid, straight to nostate  | Connecting_to_Disconnecting, Connecting -> 
      log_msg "got disconnect message before being able to connect"; Disconnecting *)

type message = 
  | Tcp_connect of string
  | Tcp_disconnect of string
  | Subscribe of string
  | Unsubscribe of string
  | Mux_failure of string
  | Schedule of string

let msg_tostring msg = 
  match msg with
  | Tcp_connect _ -> "Tcp_connect"
  | Tcp_disconnect _ -> "Tcp_disconnect"
  | Subscribe _ -> "Subscribe"
  | Unsubscribe _ -> "Unsubscribe"
  | Mux_failure _ -> "Mux_failure"
  | Schedule _ -> "Schedule"



let messages = [ 
  (1,Tcp_connect "m1");
  (2,Subscribe "m2"); 
  (3,Unsubscribe "m2");
  (4,Subscribe "m2");
  (5,Schedule "m2");
  (6,Subscribe "m2");
  (7,Mux_failure "m2"); (* check if unsubscribed *) 
  (8,Subscribe "m2");
  (9,Unsubscribe "m2");
  (10,Schedule "m2");
  (11,Unsubscribe "m2");
  (* (12,Tcp_disconnect "m1"); *)
  (13,Tcp_connect "m1");
  (14,Subscribe "m2");
  (15,Schedule "m2");

]

let debugmsg m = 
  let s = print_string ("-- " ^ m) in
  flush stdout


let cleanup state msg str =
  let _ = print_string ("cleanup (last msg: " ^ msg_tostring msg ^ " state: " ^ (state_tostring state) ^"): end of client ! \n " ^ str) in 
  flush stdout


let rec parse_event eventqueue (state: anystate): unit =
  let Any state = state in
  (* take item from list, sleep when empty *)
  match eventqueue with
   [] -> 
     let _ = debugmsg "nothing in list" in
     do_other_stuff eventqueue (Any state)
  | hd :: tail -> 
      (* print_string "something in list"; *)
      match hd with
      | (num , message) ->
          let _ = debugmsg ("\n++ new message (s: "^ state_tostring (Any state) ^") (seq: " ^ string_of_int num ^") (msg: "^msg_tostring message^")") in

          match message with
        | Tcp_connect _ -> 
            (match state with
                | Connected_subscribed ->
                    let next = next_state Connected_subscribed_to_Connecting_subscribed state in
                    parse_event tail (Any next)
                | Connecting_subscribed ->
                    parse_event tail (Any state)
                | Expectconnect ->  
                    let next = next_state Expectconnect_to_Connecting state in
                    parse_event tail (Any next)
                | Connecting ->  
                    parse_event tail (Any state)
                | Connected  ->  
                    let next = next_state Connected_to_Connecting state in
                    parse_event tail (Any next) 
                | Disconnecting ->  
                    (* only thing to do in disconnecting state is to send all messages and go to nostate *)
                    (* send all messages ... *)
                    cleanup (Any state) message "subscribe in disconnecting"
              ) (* end Tcp_connect state match *)

            | Tcp_disconnect _ -> 
              let _ = debugmsg "-- msg Tcp_disconnect \n" in
              (match state with
                | Connected_subscribed ->
                    let next = next_state Connected_subscribed_to_Disconnecting state in
                    parse_event tail (Any next)
                | Connecting_subscribed ->
                    cleanup (Any state) message "FIXME: is this correct, or should this go to disconnecting first? disconnect in connecting_subscribed"
                | Expectconnect ->  
                    cleanup (Any state) message "disconnect in expect connect"
                | Connecting ->  
                    cleanup (Any state) message "FIXME: is this correct, or should this go to disconnecting first? disconnect in connecting"
                | Connected  ->  
                    let next = next_state Connected_to_Disconnecting state in
                    parse_event tail (Any next) 
                | Disconnecting ->  
                    (* only thing to do in disconnecting state is to send all messages and go to nostate *)
                    (* send all messages ... *)
                    cleanup (Any state) message "subscribe in disconnecting"
              ) (* end Tcp_disconnect state match *)

           
            | Unsubscribe _ -> 
              (match state with
                | Connected_subscribed ->
                    let next = next_state Connected_subscribed_to_Connected state in
                    parse_event tail (Any next)
                | Connecting_subscribed ->
                    let next = next_state Connecting_subscribed_to_Connecting state in
                    parse_event tail (Any next)
                | Expectconnect ->  
                    parse_event tail (Any state)
                | Connecting ->  
                    parse_event tail (Any state)
                | Connected  ->  
                    parse_event tail (Any state)
                | Disconnecting ->  
                    (* only thing to do in disconnecting state is to send all messages and go to nostate *)
                    (* send all messages ... *)
                    cleanup (Any state) message "unsubscribe in disconnecting"
              ) (* end Tcp_disconnect state match *)

            | Mux_failure _ -> 
              (match state with
                | Connected_subscribed ->
                    let next = next_state Connected_subscribed_to_Connected state in
                    parse_event tail (Any next)
                | Connecting_subscribed ->
                    let next = next_state Connecting_subscribed_to_Connecting state in
                    parse_event tail (Any next)
                | Expectconnect ->  
                    parse_event tail (Any state)
                | Connecting ->  
                    parse_event tail (Any state)
                | Connected  ->  
                    parse_event tail (Any state)
                | Disconnecting ->  
                    (* only thing to do in disconnecting state is to send all messages and go to nostate *)
                    (* send all messages ... *)
                    cleanup (Any state) message "unsubscribe in disconnecting"
              ) (* end Tcp_disconnect state match *)
            
            | Subscribe _ -> 
                (match state with 
                    | Connected_subscribed ->
                        parse_event tail (Any state)
                    | Connecting_subscribed ->
                        parse_event tail (Any state)
                    | Expectconnect ->  
                        cleanup (Any state) message "subscribe received in expectconnect"
                    | Connecting ->  
                        let next = next_state Connecting_to_Connecting_subscribed state in
                        parse_event tail (Any next)
                    | Connected  ->  
                        let next = next_state Connected_to_Connected_subscribed state in
                        parse_event tail (Any next) 
                    | Disconnecting ->  
                        (* only thing to do in disconnecting state is to send all messages and go to nostate *)
                        (* send all messages ... *)
                        cleanup (Any state) message "subscribe in disconnecting"
                ) (* end Subscribe state match *)
            | Schedule _ -> 
                (match state with 
                    | Connected_subscribed ->
                        parse_event tail (Any state)
                    | Connecting_subscribed ->
                        parse_event tail (Any state)
                    | Expectconnect ->  
                        cleanup (Any state) message "schedule received in expectconnect"
                    | Connecting ->  
                        parse_event tail (Any state)
                    | Connected  ->  
                        parse_event tail (Any state) 
                    | Disconnecting ->  
                        (* only thing to do in disconnecting state is to send all messages and go to nostate *)
                        (* send all messages ... *)
                        cleanup (Any state) message "schedule in disconnecting"
                ) (* end Subscribe state match *)


and do_other_stuff eventqueue (state: anystate)= 
  let Any state = state in
  print_string "sleeping \n";
  flush stdout;
  Unix.sleepf 1.;
  
  parse_event eventqueue (Any state)

let parse_event eventqueue state = parse_event eventqueue (Any state)

type t = string

let t = "hello"


let test = 
  let s = Expectconnect in
(*  let s = next_state Tcp_connect s in *)
  parse_event messages s;
  (* print_string "test \n" *)


(*
  #
transition_witness :((current, next) transition))
: next state =
let next state (transition_witness : ('a,'b) transition) (t:'a state) : 'b state= ...
^---
# let next state (transition_witness : ('a,'b) transition) (t:'a state) : 'b state = match transition_witness, t with A_to_B, A -> B | B_to_A, B -> A;;


val next_state : ('current, 'next) transition -> 'current state -> 'next state = <fun>
--
# next_state B_to_A A;;
Error: This expression has type [ `a ] state but an expression was expected of type [ `b ] state These two variant types have no intersection
# next_state B_to_A B;;
- : [ `a ] state = A
# next_state A_to_B B;;
Error: This expression has type [ `b ] state but an expression was expected of type [ `a ] state These two variant types have no intersection
# next_state A_to_B A;;
- : [ `b ] state = B
etc
a

*)

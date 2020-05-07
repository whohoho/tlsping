type _ state =
  | Expectconnect : [`Expectconnect] state 
  | Connecting : [`Connecting] state
  | Connected : [`Connected] state
  | Disconnecting : [`Disconnecting] state
  | Connecting_subscribed : [`Connecting_subscribed] state
  | Connected_subscribed : [`Connected_subscribed] state

type (_,_) transition =
   | Expectconnect_to_Connecting : ([`Expectconnect], [`Connecting])  transition
(*invalid   | Connecting_to_Disconnecting : ([`Connecting], [`Disconnecting])  transition *)
   | Connecting_to_Connecting_subscribed : ([`Connecting], [`Connecting_subscribed])  transition
(* invalid   | Connecting_subscribed_to_Disconnecting : ([`Connecting_subscribed], [`Disconnecting]) transition *)
   | Connecting_subscribed_to_Connected_subscribed : ([`Connecting_subscribed], [`Connected_subscribed]) transition
   | Connected_to_Connected_subscribed : ([`Connected], [`Connected_subscribed]) transition
   | Connected_to_Disconnecting : ([`Connected], [`Disconnecting]) transition

(*   | Subscribe : ([`Connected], [`Connected_subscribed])  transition *)

let log_msg msg = 
  print_string msg;
  flush stdout

let next_state 
      (type current) (type next)
      (transition_witness :((current, next) transition))
      (t:current state) : next state 
        = 
  match transition_witness, t with 
    Connecting_subscribed_to_Connected_subscribed, Connecting_subscribed -> 
      log_msg "successfull connection (subscribed)"; Connected_subscribed
  | Connected_to_Connected_subscribed, Connected -> 
      log_msg "subscribed (connected)"; Connected_subscribed
  | Connected_to_Disconnecting, Connected -> 
      log_msg "disconnecting, waiting for queue to be emty"; Disconnecting
  | Connecting_to_Connecting_subscribed, Connecting -> 
      log_msg "subscribed (connecting)"; Connecting_subscribed
  | Expectconnect_to_Connecting, Expectconnect -> 
      log_msg "got connect message"; Connecting

(* not valid, straight to nostate  | Connecting_to_Disconnecting, Connecting -> 
      log_msg "got disconnect message before being able to connect"; Disconnecting *)

type message = 
  | Tcp_connect of string
  | Tcp_disconnect of string
  | Subscribe of string
  | Unsubscribe of string
  | Mux_failure of string
  | Schedule of string


let messages = [ 
  (1,Tcp_connect "m1");
(*  (2,Subscribe "m2"); *)
  (3,Unsubscribe "m2");
  (* (4,Subscribe "m2"); *)
  (5,Schedule "m2");
  (6,Subscribe "m2");
  (7,Mux_failure "m2"); (* check if unsubscribed *)
  (8,Subscribe "m2");
  (9,Schedule "m2");
  (10,Unsubscribe "m2");
  (11,Tcp_disconnect "m1");
  (12,Tcp_connect "m1");

]


let cleanup str =
  print_string ("end of client ! \n " ^ str)

type anystate = Any : 'a state -> anystate

let rec parse_event eventqueue (state: anystate): unit =
  let Any state = state in
  (* take item from list, sleep when empty *)
  match eventqueue with
   [] -> 
      print_string "nothing in list";
      do_other_stuff eventqueue (Any state)
    | hd :: tail -> 
      print_string "something in list";
      match hd with
      | (num , message) ->
        print_string ("new message: " ^ string_of_int num ^ "\n");
        match message with
        | Tcp_connect _ -> 
          print_string "-- msg Tcp_connect \n";
          flush stdout;
          parse_event tail (Any state)
        | Tcp_disconnect _ -> 
          print_string "-- msg Tcp_disconnect \n";
          flush stdout;
          parse_event tail (Any state)
        | Unsubscribe _ -> 
          print_string "-- msg Unsubscribe \n";
          flush stdout;
          parse_event tail (Any state)
        | Mux_failure _ -> 
          print_string "-- msg Mux_failure \n";
          flush stdout;
          parse_event tail (Any state)
        | Schedule _ -> 
          print_string "-- msg Schedule \n";
          flush stdout;
          parse_event tail (Any state)
        | Subscribe _ -> 
           print_string "-- msg Subscribe \n";
           flush stdout;
          match state with
            | Connected_subscribed ->
              parse_event tail (Any state)
            | Connecting_subscribed ->
              parse_event tail (Any state)
            | Expectconnect ->  
              cleanup "subscribe received in expectconnect"
            | Connecting ->  
              let next = next_state Connecting_to_Connecting_subscribed state in
              parse_event tail (Any next)
            | Connected  ->  
              let next = next_state Connected_to_Connected_subscribed state in
              parse_event tail (Any next) 
            | Disconnecting ->  
              (* only thing to do in disconnecting state is to send all messages and go to nostate *)
              (* send all messages ... *)
              cleanup "subscribe in disconnecting"
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
  print_string "test \n"


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

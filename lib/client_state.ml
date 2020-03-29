type _ state =
  | Expectconnect : [`Expectconnect] state 
  | Connecting : [`Connecting] state
  | Connected : [`Connected] state
  | Disconnecting : [`Disconnecting] state
  | Connecting_subscribed : [`Connecting_subscribed] state
  | Connected_subscribed : [`Connected_subscribed] state

type (_,_) transition =
   | Expectconnect_to_Connecting : ([`Expectconnect], [`Connecting])  transition
   | Connecting_to_Connecting_subscribed : ([`Connecting], [`Connecting_subscribed])  transition
   | Connecting_subscribed_to_Connected_subscribed : ([`Connecting_subscribed], [`Connected_subscribed]) transition
   | Connected_to_Connected_subscribed : ([`Connected], [`Connected_subscribed]) transition
(*   | Subscribe : ([`Connected], [`Connected_subscribed])  transition *)


let next_state 
      (type current) (type next)
      (transition_witness :((current, next) transition))
      (t:current state) : next state 
        = 
  match transition_witness, t with 
    Connecting_subscribed_to_Connected_subscribed, Connecting_subscribed-> print_string "successfull connection"; Connected_subscribed
  | Connected_to_Connected_subscribed, Connected -> print_string "subscribed"; Connected_subscribed
  | Connecting_to_Connecting_subscribed, Connecting -> print_string "subscribed"; Connecting_subscribed
  | Expectconnect_to_Connecting, Expectconnect -> print_string "got connect message"; Connecting


type message = 
  | Tcp_connect of string
  | Subscribe of string

let messages = [ 
  (1,Tcp_connect "m1");
  (1,Subscribe "m2")
]


let cleanup =
  print_string "end of client ! \n "
let rec what (type fuck) (t :fuck state) : _ state = t
and parse_event eventqueue (type fuck) (type actual) (state: actual state): unit =
  (* take item from list, sleep when empty *)
  match eventqueue with
    [] -> () (*(do_other_stuff : int list -> actual state -> unit) eventqueue state*)
    | hd :: tail -> 
      match hd with
(*        _ -> print_string "This was not a valid message"; *)
      | (_ , message) ->
        match message with
        Tcp_connect _ -> ()
        | Subscribe _ -> 
        
          match state with
            | Connecting ->  
              let next = next_state Connecting_to_Connecting_subscribed state in
              parse_event tail (what next)
            | Connected  ->  
              let next = next_state Connected_to_Connected_subscribed state in
              parse_event tail (what next) 
            | Disconnecting -> .
            | Disconnecting ->  
              (* only thing to do in disconnecting state is to send all messages and go to nostate *)
              (* send all messages ... *)
              cleanup
and do_other_stuff eventqueue state= 
  print_string "sleeping \n";
  Unix.sleepf 12000.;
  parse_event eventqueue state



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

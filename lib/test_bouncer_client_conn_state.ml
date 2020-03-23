open Alcotest

let cs = Bouncer_client_conn_state 

let test_serialization () = 
  (check string) "serializing 1" "" ""


(*  Alcotest.(check string) "items in map" "whtat is dit" serialized_state () *)


let () =

  run "Utils" [
      "map", [ test_case "serialization" `Quick test_serialization  ];
    ]

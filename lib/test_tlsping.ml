open Alcotest


let test_serialization () = 
  let ss1 = Shared.read_file "serialized_state.txt" in
  let state = Shared.deserialize_tls_state ss1 in
  let ss2 = Shared.serialize_tls_state ~sanity:false state in
  (check string) "serializing 1" ss1 ss2


(*  Alcotest.(check string) "items in map" "whtat is dit" serialized_state () *)


let () =
  print_string "disabled"
(*
  run "Utils" [
      "map", [ test_case "serialization" `Quick test_serialization  ];
    ]
   *)

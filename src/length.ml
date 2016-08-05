(* 
 + The problem:
     Reverse a list
 
 + Source: 
     https://ocaml.org/learn/tutorials/99problems.html  

 + Compilation guide:
     ocamlbuild -pkgs oUnit length.byte
*)

open OUnit

(******************************)
(***        solution        ***)

(* tail recursion using accumulator *)
let length xs =
  let rec get_length acc = function
    | [] -> acc
    | _::vs -> get_length (acc + 1) vs in
  get_length 0 xs

(******************************)
(***        unit test       ***)

let test_length () =
  assert_equal (0) (length []);
  assert_equal (3) (length [1; 2; 3]);
  assert_equal (1) (length [false])

let suite = "Testing" >::: ["length" >:: test_length]

(* main function *)
let _ =
  run_test_tt_main suite


(* 
 + The problem:
     Find the last_two but one (last_two and penultimate) elements of a list.
 
 + Source: 
     https://ocaml.org/learn/tutorials/99problems.html  

 + Compilation guide:
     ocamlbuild -pkgs oUnit last_two.byte
*)

open OUnit

(******************************)
(***        solution        ***)

let rec last_two = function
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: xs -> last_two xs

(******************************)
(***        unit test       ***)

let test_last () =
  assert_equal (None)   (last_two []);
  assert_equal (Some (2, 3))   (last_two [1; 2; 3]);
  assert_equal (Some ("d", "a")) (last_two ["c"; "d"; "a"]);
  assert_equal (Some (true, true)) (last_two [false; true; true]);
  assert_equal (None) (last_two [false])

let suite = "Testing" >::: ["last_two" >:: test_last]

(* main function *)
let _ =
  run_test_tt_main suite


(* 
 Created by : Ta Quang Trung (taquangtrungvn(at)gmail.com)

 + The problem:
     Write a function "last : 'a list -> 'a option"
     that returns the last element of a list.
 
 + Source: 
     https://ocaml.org/learn/tutorials/99problems.html  

 + Compilation guide:
     ocamlbuild -use-ocamlfind -pkgs oUnit last.byte

*)

open OUnit

(******************************)
(***        solution        ***)
let rec last = function
  | [] -> None
  | x::[] -> Some x
  | x::xs -> last xs

(******************************)
(***        unit test       ***)

let test_last () =
  assert_equal (None)   (last []);
  assert_equal (Some 3)   (last [1; 2; 3]);
  assert_equal (Some "a") (last ["c"; "d"; "a"]);
  assert_equal (Some true) (last [false; true; true])

let suite = "Testing" >::: ["last" >:: test_last]

(* main function *)
let _ =
  run_test_tt_main suite


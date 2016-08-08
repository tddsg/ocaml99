(*
 + The problem:
     Find out whether a list is a palindrome. (easy)

 + Source:
     https://ocaml.org/learn/tutorials/99problems.html

 + Compilation guide:
     ocamlbuild -use-ocamlfind -pkgs oUnit is_palindrome.byte
*)

open OUnit

(******************************)
(***        solution        ***)

let is_palindrome lst =
  lst = List.rev lst

(******************************)
(***        unit test       ***)

let test_palindrome () =
  assert_equal (true)   (is_palindrome []);
  assert_equal (true)   (is_palindrome [1; 2; 3; 2 ;1]);
  assert_equal (true) (is_palindrome [true; false; true])

let suite = "Testing" >::: ["is_palindrome" >:: test_palindrome]

(* main function *)
let _ =
  run_test_tt_main suite


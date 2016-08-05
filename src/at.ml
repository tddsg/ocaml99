(* 
 + The problem:
     Find the k'th element of a list. (easy)
 
 + Source: 
     https://ocaml.org/learn/tutorials/99problems.html  

 + Compilation guide:
     ocamlbuild -pkgs oUnit at.byte
*)

open OUnit

(******************************)
(***        solution        ***)

let rec at k xs =
  match xs with
  | [] -> None
  | v::vs ->
    if (k < 0) then None
    else if (k = 1) then Some v
    else at (k - 1) vs

(******************************)
(***        unit test       ***)

let test_at () =
  assert_equal (None)   (at 1 []);
  assert_equal (Some 2)   (at 2 [1; 2; 3]);
  assert_equal (Some "a") (at 3 ["c"; "d"; "a"]);
  assert_equal (None) (at 5 [false; true; true]);
  assert_equal (None) (at (-1) [false])

let suite = "Testing" >::: ["at" >:: test_at]

(* main function *)
let _ =
  run_test_tt_main suite


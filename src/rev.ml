(* 
 + The problem:
     Reverse a list
 
 + Source: 
     https://ocaml.org/learn/tutorials/99problems.html  

 + Compilation guide:
     ocamlbuild -pkgs oUnit rev.byte
*)

open OUnit

(******************************)
(***        solution        ***)

(* tail recursion using accumulator *)
let rev xs =
  let rec do_rev acc = function
    | [] -> acc
    | x :: xs -> do_rev (x :: acc) xs in
  do_rev [] xs

(******************************)
(***        unit test       ***)

let test_rev () =
  assert_equal ([]) (rev []);
  assert_equal ([3; 2; 1]) (rev [1; 2; 3]);
  assert_equal ([false]) (rev [false])

let suite = "Testing" >::: ["rev" >:: test_rev]

(* main function *)
let _ =
  run_test_tt_main suite


(*
 + The problem:
     Flatten a nested list structure. (medium)

 + Source:
     https://ocaml.org/learn/tutorials/99problems.html

 + Compilation guide:
     ocamlbuild -pkgs oUnit flatten.byte
*)

open OUnit

(******************************)
(***        solution        ***)

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten lst =
  let rec flat = function
    | One x -> [x]
    | Many xs -> fold [] xs
  and fold acc = function
    | [] -> acc
    | x :: xs -> fold (acc @ (flat x)) xs in
  fold [] lst

(******************************)
(***        unit test       ***)

let test_flatten () =
  assert_equal ["a"; "b"; "c"; "d"; "e"]
    (flatten [One "a" ; Many [One "b" ; Many [One "c" ; One "d"] ; One "e"] ]);
  assert_equal ["a"; "b"; "c"; "d"; "e"]
    (flatten [One "a" ; Many [One "b" ; One "c"; Many [One "d"] ; One "e"] ])


let suite = "Testing" >::: ["flatten" >:: test_flatten]

(* main function *)
let _ =
  run_test_tt_main suite


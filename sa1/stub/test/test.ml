open OUnit2
open Recs.Interp
open Recs.Ds

(* A few test cases *)
let tests_let = [
  "int"  >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "22"));
  "add"  >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "11+11"));
  "adds" >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "(10+1)+(5+6)"));
  "let"  >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "let x=22 in x"));
  "lets" >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "let x = 0 in let x = 22 in x"));
]


let tests_proc = [
  "int"  >:: (fun _ -> assert_equal (Ok (NumVal 3))
                 (interp "(proc (x) { x+1 } 2)"))
]

let tests_rec = [
  "int"  >:: (fun _ -> assert_equal (Ok (NumVal 120))
                 (interp "letrec f(x) = if zero?(x) then 1 else x*(f (x-1))
in (f 5)"))
]

let tests_recht = [
  "stack1"  >:: (fun _ -> assert_equal (Ok (StackVal []))
                 (interp "emptystack"))
  ;
  "stack2"  >:: (fun _ -> assert_equal (Ok (StackVal [NumVal 1; NumVal 2; NumVal 3]))
                 (interp "push(1, push(2, push(3, emptystack)))"))
  ;
    "stack3"  >:: (fun _ -> assert_equal (Ok (StackVal [NumVal 2; NumVal 3]))
                      (interp
                         "pop(push(1, push(2, push(3, emptystack))))"))
  ;
    "stack4"  >:: (fun _ -> assert_equal (Ok (NumVal 1))
                      (interp
                         "top(push(1, push(2, push(3, emptystack))))"))
  ;
    "stack5"  >:: (fun _ -> assert_equal (Ok (NumVal 3))
                      (interp
                          "size(push(1, push(2, push(3, emptystack))))"))
  ;
     "stack6"  >:: (fun _ -> assert_equal ( Ok (BoolVal false))
                      (interp
                           "empty?(push(1, push(2, push(3, emptystack))))"))
]


let _ = run_test_tt_main ("suite" >::: (tests_let @ tests_proc
                                        @ tests_rec @ tests_recht ))

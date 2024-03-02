open OUnit2
open Ninety_nine

let () = run_test_tt_main (
    "Lists Problem 01" >:::
    [ "last on empty list" >:: (fun _ ->
          assert_equal None (Lists.last []))
    ; "last on 1 elem list" >:: (fun _ ->
          assert_equal (Some 1) (Lists.last [1]))
    ; "last on 5 elem list" >:: (fun _ ->
          assert_equal (Some 5) (Lists.last [1; 2; 3; 4; 5]))
    ]
  )

let () = run_test_tt_main (
    "Lists: Problem 02" >:::
    [ "last_two on empty list" >:: (fun _ ->
          assert_equal None (Lists.last_two []))
    ; "last_two on 1 elem list" >:: (fun _ ->
          assert_equal None (Lists.last_two [1]))
    ; "last_two on 2 elem list" >:: (fun _ ->
          assert_equal (Some [1; 2]) (Lists.last_two [1; 2]))
    ;  "last_two on 5 elem list" >:: (fun _ ->
          assert_equal (Some [4;  5]) (Lists.last_two [1; 2; 3; 4; 5]))
    ]
  )

let () = run_test_tt_main (
    "Lists: Problem 03" >:::
    [ "at on empty list pos 3" >:: (fun _ ->
          assert_raises (Failure "Index out of bounds") (fun () -> Lists.at 3 []))
    ; "at on 2 elem list pos 3" >:: (fun _ ->
          assert_raises (Failure "Index out of bounds") (fun () -> Lists.at 3 [1; 2]))
    ; "at on 5 elem list pos 3" >:: (fun _ ->
          assert_equal 4 (Lists.at 3 [1; 2; 3; 4; 5]))
    ; "at' on empty list pos 3" >:: (fun _ ->
          assert_equal None (Lists.at' 3 []))
    ; "at' on 2 elem list pos 3" >:: (fun _ ->
          assert_equal None (Lists.at' 3 [1; 2]))
    ; "at' on 5 elem list pos 3" >:: (fun _ ->
          assert_equal (Some 4) (Lists.at' 3 [1; 2; 3; 4; 5]))
    ]
  )

let () = run_test_tt_main (
    "Lists: Problem 04" >:::
    [ "length on a empty list" >:: (fun _ ->
          assert_equal 0 (Lists.length []))
    ; "length on a 1 elem list" >:: (fun _ ->
          assert_equal 1 (Lists.length [1]))
    ;  "length on a 5 elem list" >:: (fun _ ->
          assert_equal 5 (Lists.length [1; 2; 3; 4; 5]))
    ]
  )

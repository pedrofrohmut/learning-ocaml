open OUnit2
open Ninety_nine

let () = run_test_tt_main (
  "Lists Problem 01" >:::
  [ "last on empty list" >:: (fun _ ->
        assert_equal None (Lists.last []))

  ; "last on 1 elem list" >:: (fun _ ->
        assert_equal (Some 1) (Lists.last [1]))

  ; "last on 5 elem list" >:: (fun _ ->
        assert_equal (Some 5) (Lists.last [1;2;3;4;5]))
  ]
)

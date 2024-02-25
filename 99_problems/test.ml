open OUnit2
open Sum

let tests = "Tests for function sum" >::: [
  "empty" >:: (fun _ ->  assert_equal 0 (sum []));
  "singleton" >:: (fun _ -> assert_equal 1 (sum [1]));
  "two_elements" >:: (fun _ -> assert_equal 3 (sum [1; 2]));
]

let () = run_test_tt_main tests

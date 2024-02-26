open OUnit2
open Main

let _tests1 =
  "Test for problem 01" >::: [
    "empty list" >:: (fun _ ->
        assert_equal None (my_last []));

    "one element list" >:: (fun _ ->
        assert_equal (Some 2) (my_last [2]));

    "more than one element list" >:: (fun _ ->
        assert_equal (Some 3) (my_last [1; 2; 3]))
  ]

let _tests2 =
  "Tests for problem 02" >::: [
    "Empty list" >:: (fun _ ->
        assert_equal None (my_penultimate []));

    "One element list" >:: (fun _ ->
        assert_equal None (my_penultimate [1]));

    "Two element list" >:: (fun _ ->
        assert_equal (Some 1) (my_penultimate [1; 2]));

    "More than two element list" >:: (fun _ ->
        assert_equal (Some 2) (my_penultimate [1; 2; 3]))
  ]

let _tests3 =
  "Tests for problem 03" >::: [
    "Empty list and position 1" >:: (fun _ ->
        assert_equal None (my_at [] 1));

    "One elem list and pos 1" >:: (fun _ ->
        assert_equal None (my_at [1] 1));

    "Two elem list and pos 1" >:: (fun _ ->
        assert_equal (Some 2) (my_at [1; 2] 1));

    "More than two elem list pos 1" >:: (fun _ ->
        assert_equal (Some 2) (my_at [1; 2; 3; 4; 5] 1));

    "More than two elem list pos 3" >:: (fun _ ->
        assert_equal (Some 4) (my_at [1; 2; 3; 4; 5] 3))
  ]

let _tests4 =
  "Tests for problem 04" >::: [
    "Empty list" >:: (fun _ ->
        assert_equal 0 (my_length []));
    "One elem list" >:: (fun _ ->
        assert_equal 1 (my_length [1]));
    "Six elem list" >:: (fun _ ->
        assert_equal 6 (my_length [1; 2; 3; 4; 5; 6]))
  ]

let _tests5 =
  "Tests for problem 05" >::: [
    "Empty list" >:: (fun _ ->
        assert_equal [] (my_reverse []));
    "One elem list" >:: (fun _ ->
        assert_equal [1] (my_reverse [1]));
    "Two elem list" >:: (fun _ ->
        assert_equal [2; 1] (my_reverse [1; 2]));
    "Six elem list" >:: (fun _ ->
        assert_equal [6; 5; 4; 3; 2; 1] (my_reverse [1; 2; 3; 4; 5; 6]));
  ]

let _tests6 =
  "Tests for problem 06" >::: [
    "Empty list" >:: (fun _ ->
        assert_equal true (my_is_palindrome []));

    "One elem list" >:: (fun _ ->
        assert_equal true (my_is_palindrome [1]));

    "Two elem list" >:: (fun _ ->
        assert_equal true (my_is_palindrome [1; 1]));

    "Two elem list but different" >:: (fun _ ->
        assert_equal false (my_is_palindrome [1; 2]));

    "Six elem list" >:: (fun _ ->
        assert_equal true (my_is_palindrome [1; 2; 3; 3; 2; 1]));

    "Six elem list but different" >:: (fun _ ->
        assert_equal false (my_is_palindrome [1; 2; 3; 4; 5; 6]));
  ]

let tests7 =
  "Tests for problem 07" >::: [
    "Empty list" >:: (fun _ ->
        assert_equal [] (my_flatten []));

    "One elem list" >:: (fun _ ->
        assert_equal [1] (my_flatten [One 1]));

    "Three elem list" >:: (fun _ ->
        assert_equal [1; 2; 3] ( my_flatten [One 1; One 2; One 3]));

    "Many list into simple list" >:: (fun _ ->
        assert_equal [1; 2; 3] (my_flatten [Many [One 1; One 2; One 3]]));

    "Example list" >:: (fun _ ->
        assert_equal
          ["a"; "b"; "c"; "d"; "e"]
          (my_flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]));
  ]

let () = run_test_tt_main tests7

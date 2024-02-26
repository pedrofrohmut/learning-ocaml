open OUnit2

let tests1 =
  "Test for problem 01" >::: [
    "empty list" >:: (fun _ ->
        assert_equal None (Lists.last []));

    "one element list" >:: (fun _ ->
        assert_equal (Some 2) (Lists.last [2]));

    "more than one element list" >:: (fun _ ->
        assert_equal (Some 3) (Lists.last [1; 2; 3]))
  ]

let () = run_test_tt_main tests1

let tests1_1 =
  "Tests for problem 01 extra" >::: [
    "Empty list" >:: (fun _ ->
        assert_equal None (Lists.penultimate []));

    "One element list" >:: (fun _ ->
        assert_equal None (Lists.penultimate [1]));

    "Two element list" >:: (fun _ ->
        assert_equal (Some 1) (Lists.penultimate [1; 2]));

    "More than two element list" >:: (fun _ ->
        assert_equal (Some 2) (Lists.penultimate [1; 2; 3]))
  ]

let () = run_test_tt_main tests1_1

let tests2 =
  "Tests for problem 02" >::: [
    "Empty list" >:: (fun _ ->
        assert_equal None (Lists.last_two []));

    "One elem list" >:: (fun _ ->
        assert_equal None (Lists.last_two [1]));

    "Two elem list" >:: (fun _ ->
        assert_equal (Some (1, 2)) (Lists.last_two [1; 2]));

    "Six elem list" >:: (fun _ ->
        assert_equal (Some (5, 6)) (Lists.last_two [1; 2; 3; 4; 5; 6]));
  ]

let () = run_test_tt_main tests2

let tests3 =
  "Tests for problem 03" >::: [
    "Empty list and position 1" >:: (fun _ ->
        assert_equal None (Lists.at [] 1));

    "One elem list and pos 1" >:: (fun _ ->
        assert_equal None (Lists.at [1] 1));

    "Two elem list and pos 1" >:: (fun _ ->
        assert_equal (Some 2) (Lists.at [1; 2] 1));

    "More than two elem list pos 1" >:: (fun _ ->
        assert_equal (Some 2) (Lists.at [1; 2; 3; 4; 5] 1));

    "More than two elem list pos 3" >:: (fun _ ->
        assert_equal (Some 4) (Lists.at [1; 2; 3; 4; 5] 3))
  ]

let () = run_test_tt_main tests3

let tests4 =
  "Tests for problem 04" >::: [
    "Empty list" >:: (fun _ ->
        assert_equal 0 (Lists.length []));
    "One elem list" >:: (fun _ ->
        assert_equal 1 (Lists.length [1]));
    "Six elem list" >:: (fun _ ->
        assert_equal 6 (Lists.length [1; 2; 3; 4; 5; 6]))
  ]

let () = run_test_tt_main tests4

let tests5 =
  "Tests for problem 05" >::: [
    "Empty list" >:: (fun _ ->
        assert_equal [] (Lists.reverse []));
    "One elem list" >:: (fun _ ->
        assert_equal [1] (Lists.reverse [1]));
    "Two elem list" >:: (fun _ ->
        assert_equal [2; 1] (Lists.reverse [1; 2]));
    "Six elem list" >:: (fun _ ->
        assert_equal [6; 5; 4; 3; 2; 1] (Lists.reverse [1; 2; 3; 4; 5; 6]));
  ]

let () = run_test_tt_main tests5

let tests6 =
  "Tests for problem 06" >::: [
    "Empty list" >:: (fun _ ->
        assert_equal true (Lists.is_palindrome []));

    "One elem list" >:: (fun _ ->
        assert_equal true (Lists.is_palindrome [1]));

    "Two elem list" >:: (fun _ ->
        assert_equal true (Lists.is_palindrome [1; 1]));

    "Two elem list but different" >:: (fun _ ->
        assert_equal false (Lists.is_palindrome [1; 2]));

    "Six elem list" >:: (fun _ ->
        assert_equal true (Lists.is_palindrome [1; 2; 3; 3; 2; 1]));

    "Six elem list but different" >:: (fun _ ->
        assert_equal false (Lists.is_palindrome [1; 2; 3; 4; 5; 6]));
  ]

let () = run_test_tt_main tests6

let tests7 =
  "Tests for problem 07" >::: [
    "Empty list" >:: (fun _ ->
        assert_equal [] (Lists.flatten []));

    "One elem list" >:: (fun _ ->
        assert_equal [1] (Lists.flatten [One 1]));

    "Three elem list" >:: (fun _ ->
        assert_equal [1; 2; 3] ( Lists.flatten [One 1; One 2; One 3]));

    "Many list into simple list" >:: (fun _ ->
        assert_equal [1; 2; 3] (Lists.flatten [Many [One 1; One 2; One 3]]));

    "Example list" >:: (fun _ ->
        assert_equal
          ["a"; "b"; "c"; "d"; "e"]
          (Lists.flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]));
  ]


let () = run_test_tt_main tests7

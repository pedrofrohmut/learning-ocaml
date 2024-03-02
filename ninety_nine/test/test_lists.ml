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

let () = run_test_tt_main (
    "Lists: Problem 05" >:::
    [ "rev on an empty list" >:: (fun _ ->
          assert_equal [] (Lists.rev []))
    ; "rev on 1 elem list" >:: (fun _ ->
          assert_equal [1] (Lists.rev [1]))
    ; "rev on 5 elem list" >:: (fun _ ->
          assert_equal [5; 4; 3; 2; 1] (Lists.rev [1; 2; 3; 4; 5]))
    ]
  )

let () = run_test_tt_main (
    "Lists: Problem 06" >:::
    [ "is_palindrome on a empty list" >:: (fun _ ->
          assert_equal true (Lists.is_palindrome []))
    ; "is_palindrome on a 1 elem list" >:: (fun _ ->
          assert_equal true (Lists.is_palindrome [1]))
    ; "is_palindrome on a 5 elem palindrome list" >:: (fun _ ->
          assert_equal true (Lists.is_palindrome [1; 2; 3; 2; 1]))
    ; "is_palindrome on a 2 elem not palindrome list" >:: (fun _ ->
          assert_equal false (Lists.is_palindrome [1; 2]))
    ]
  )

let () = run_test_tt_main (
    "Lists: Problem 07" >:::
    [ "flatten on a empty list" >:: (fun _ ->
          assert_equal [] (Lists.flatten []))
    ; "flatten on a 1 elem list with One x" >:: (fun _ ->
          assert_equal [1] (Lists.flatten [One 1]))
    ; "flatten on a 5 elem list with Many (One x; ...)" >:: (fun _ ->
          assert_equal
            [1; 2; 3; 4; 5]
            (Lists.flatten [Many [One 1; One 2; One 3; One 4; One 5]]))
    ; "flatten on a more complex example" >:: (fun _ ->
          assert_equal
            ["a"; "b"; "c"; "d"; "e"]
            (Lists.flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]))
    ]
  )

let () = run_test_tt_main (
    "Lists: Problem 08" >:::
    [ "compress on a empty list" >:: (fun _ ->
          assert_equal [] (Lists.compress []))
    ; "compress on a 1 elem list" >:: (fun _ ->
          assert_equal [1] (Lists.compress [1]))
    ; "compress on a 2 elem list with repeated values" >:: (fun _ ->
          assert_equal [1] (Lists.compress [1; 1]))
    ; "compress on a 5 elem list" >:: (fun _ ->
          assert_equal [1; 2; 3; 4; 5] (Lists.compress [1; 2; 3; 4; 5]))
    ; "compress on a more complex example" >:: (fun _ ->
          assert_equal
            ["a"; "b"; "c"; "a"; "d"; "e"]
            (Lists.compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]))
    ]
  )

let () = run_test_tt_main (
    "Lists: Problem 09" >:::
    [ "pack on a empty list" >:: (fun _ ->
          assert_equal [] (Lists.pack []))
    ; "pack on a 1 elem list" >:: (fun _ ->
          assert_equal [[1]] (Lists.pack [1]))
    ; "pack on a 3 elem list" >:: (fun _ ->
          assert_equal [[1]; [2]; [3]] (Lists.pack [1; 2; 3]))
    ; "pack on a 2 elem list with repeated values" >:: (fun _ ->
          assert_equal [[1; 1]] (Lists.pack [1; 1]))
    ; "pack on a 5 elem list with repeated values" >:: (fun _ ->
          assert_equal [[1; 1]; [2; 2; 2]] (Lists.pack [1; 1; 2; 2; 2]))
    ; "pack on a more complex example" >:: (fun _ ->
          assert_equal
            [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]
            (Lists.pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]))
    ]
  )

let () = run_test_tt_main (
    "Lists: Problem 10" >:::
    [ "encode on a empty list" >:: (fun _ ->
          assert_equal [] (Lists.encode []))
    ; "encode on a 1 elem list" >:: (fun _ ->
          assert_equal [(1, "a")] (Lists.encode ["a"]))
    ; "encode on a 3 elem list with different values" >:: (fun _ ->
          assert_equal [(1, "a"); (1, "b"); (1, "c")] (Lists.encode ["a"; "b"; "c"]))
    ; "encode on a 3 elem list with same values" >:: (fun _ ->
          assert_equal [(3, "a")] (Lists.encode ["a"; "a"; "a"]))
    ; "encode on a 5 elem list with 2 values but they repeats" >:: (fun _ ->
          assert_equal [(3, "a"); (2, "b")] (Lists.encode ["a"; "a"; "a"; "b"; "b"]))
    ; "encode on a more complex example" >:: (fun _ ->
          assert_equal
            [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
            (Lists.encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]))
    ]
  )

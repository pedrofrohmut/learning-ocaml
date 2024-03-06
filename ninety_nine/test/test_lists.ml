open OUnit2
open Ninety_nine

let () = run_test_tt_main (
    "Lists Problem 01" >:::
    [ "last on an empty list" >:: (fun _ ->
          assert_equal None (Lists.last []))
    ; "last on 1 elem list" >:: (fun _ ->
          assert_equal (Some 1) (Lists.last [1]))
    ; "last on 5 elem list" >:: (fun _ ->
          assert_equal (Some 5) (Lists.last [1; 2; 3; 4; 5]))
    ]
  )

let () = run_test_tt_main (
    "Lists: Problem 02" >:::
    [ "last_two on an empty list" >:: (fun _ ->
          assert_equal None (Lists.last_two []))
    ; "last_two on 1 elem list" >:: (fun _ ->
          assert_equal None (Lists.last_two [1]))
    ; "last_two on 2 elem list" >:: (fun _ ->
          assert_equal (Some (1, 2)) (Lists.last_two [1; 2]))
    ;  "last_two on 5 elem list" >:: (fun _ ->
          assert_equal (Some (4, 5)) (Lists.last_two [1; 2; 3; 4; 5]))
    ]
  )

let () = run_test_tt_main (
    "Lists: Problem 03" >:::
    [ "at on an empty list pos 3" >:: (fun _ ->
          assert_raises (Failure "Index out of bounds") (fun () -> Lists.at 3 []))
    ; "at on 2 elem list pos 3" >:: (fun _ ->
          assert_raises (Failure "Index out of bounds") (fun () -> Lists.at 3 [1; 2]))
    ; "at on 5 elem list pos 3" >:: (fun _ ->
          assert_equal 4 (Lists.at 3 [1; 2; 3; 4; 5]))
    ; "at' on an empty list pos 3" >:: (fun _ ->
          assert_equal None (Lists.at_opt 3 []))
    ; "at' on 2 elem list pos 3" >:: (fun _ ->
          assert_equal None (Lists.at_opt 3 [1; 2]))
    ; "at' on 5 elem list pos 3" >:: (fun _ ->
          assert_equal (Some 4) (Lists.at_opt 3 [1; 2; 3; 4; 5]))
    ]
  )

let () = run_test_tt_main (
    "Lists: Problem 04" >:::
    [ "length on an empty list" >:: (fun _ ->
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
    [ "is_palindrome on an empty list" >:: (fun _ ->
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
    [ "flatten on an empty list" >:: (fun _ ->
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
    [ "compress on an empty list" >:: (fun _ ->
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
    [ "pack on an empty list" >:: (fun _ ->
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
    [ "encode on an empty list" >:: (fun _ ->
          assert_equal [] (Lists.encode []))
    ; "encode on a 1 elem list" >:: (fun _ ->
          assert_equal [(1, "a")] (Lists.encode ["a"]))
    ; "encode on a 3 elem list with different values" >:: (fun _ ->
          assert_equal [(1, "a"); (1, "b"); (1, "c")] (Lists.encode ["a"; "b"; "c"]))
    ; "encode on a 3 elem list with same values" >:: (fun _ ->
          assert_equal [(3, "a")] (Lists.encode ["a"; "a"; "a"]))
    ; "encode on a 5 elem list with 2 diff values but they repeat" >:: (fun _ ->
          assert_equal [(3, "a"); (2, "b")] (Lists.encode ["a"; "a"; "a"; "b"; "b"]))
    ; "encode on a more complex example" >:: (fun _ ->
          assert_equal
            [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
            (Lists.encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]))
    ]
  )

let () = run_test_tt_main (
    let open Lists in
    "Lists: Problem 11" >:::
    [ "encode2 on an empty list" >:: (fun _ ->
          assert_equal [] (Lists.encode2 []))
    ; "encode2 on 1 elem list" >:: (fun _ ->
          assert_equal [EncOne 1] (Lists.encode2 [1]))
    ; "encode2 on 3 elem list no repeated values" >:: (fun _ ->
          assert_equal [EncOne 1; EncOne 2; EncOne 3] (Lists.encode2 [1; 2; 3]))
    ; "encode2 on 5 elem list with 2 diff values but they repeat" >:: (fun _ ->
          assert_equal [EncMany (3, "a"); EncMany (2, "b")] (Lists.encode2 ["a"; "a"; "a"; "b"; "b"]))
    ; "encode2 on a more complex example" >:: (fun _ ->
          assert_equal
            [EncMany (4, "a"); EncOne "b"; EncMany (2, "c"); EncMany (2, "a"); EncOne "d"; EncMany (4, "e")]
            (Lists.encode2 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]))
    ]
  )

let () = run_test_tt_main (
    "Lists: Problem 12" >:::
    [ "decode on an empty list" >:: (fun _ ->
          assert_equal [] (Lists.decode []))
    ; "decode on a 1 elem list with EncOne 1" >:: (fun _ ->
          assert_equal [1] (Lists.decode [EncOne 1]))
    ; "decode on a 1 elem list with EncMany (3, 1)" >:: (fun _ ->
          assert_equal [1; 1; 1] (Lists.decode [EncMany (3, 1)]))
    ; "decode on a 3 elem list of EncOne" >:: (fun _ ->
          assert_equal [1; 2; 3] (Lists.decode [EncOne 1; EncOne 2; EncOne 3]))
    ; "decode on a 2 elem list of EncMany" >:: (fun _ ->
          assert_equal ["a"; "a"; "a"; "b"; "b"] (Lists.decode [EncMany (3, "a"); EncMany (2, "b")]))
    ; "decode on a more complex example" >:: (fun _ ->
          assert_equal
            ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
            (Lists.decode [EncMany (4, "a"); EncOne "b"; EncMany (2, "c"); EncMany (2, "a");
                           EncOne "d"; EncMany (4, "e")]))
    ]
  )

(* Skipped the problem 13 *)

let () = run_test_tt_main (
    "Lists: Problem 14" >:::
    [ "duplicate on an empty list" >:: (fun _ ->
          assert_equal [] (Lists.duplicate []))
    ; "duplicate on 1 elem list" >:: (fun _ ->
          assert_equal [1; 1] (Lists.duplicate [1]))
    ; "duplicate on a 3 elem" >:: (fun _ ->
          assert_equal [1; 1; 2; 2; 3;  3] (Lists.duplicate [1; 2; 3]))
    ; "duplicate on a more complex example" >:: (fun _ ->
          assert_equal
            ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
            (Lists.duplicate ["a"; "b"; "c"; "c"; "d"]))
    ]
  )

let () = run_test_tt_main (
    "Lists: Problem 15" >:::
    [ "replicate on an empty list" >:: (fun _ ->
          assert_equal [] (Lists.replicate 3 []))
    ; "replicate on 1 elem list times 3" >:: (fun _ ->
          assert_equal [1; 1; 1] (Lists.replicate 3 [1]))
    ; "replicate on 3 elem list times 3" >:: (fun _ ->
          assert_equal [1; 1; 1; 2; 2; 2; 3; 3; 3] (Lists.replicate 3 [1; 2; 3]))
    ; "replicate on a more complex example" >:: (fun _ ->
          assert_equal
            ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
            (Lists.replicate 3 ["a"; "b"; "c"]))
    ]
  )

let () = run_test_tt_main (
    "Lists: Problem 16" >:::
    [ "drop 3 on an empty list" >:: (fun _ ->
          assert_equal [] (Lists.drop 3 []))
    ; "drop 3 on 1 elem list" >:: (fun _ ->
          assert_equal [1] (Lists.drop 3 [1]))
    ; "drop 3 on 3 elem list" >:: (fun _ ->
          assert_equal [1; 2] (Lists.drop 3 [1; 2; 3]))
    ; "drop 3 on 6 elem list" >:: (fun _ ->
          assert_equal [1; 2; 4; 5] (Lists.drop 3 [1; 2; 3; 4; 5; 6]))
    ; "drop on a more complex example" >:: (fun _ ->
          assert_equal
            ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
            (Lists.drop 3 ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"]))
    ]
  )

let () = run_test_tt_main (
    "Lists: Problem 17" >:::
    [ "split 3 on an empty list" >:: (fun _ ->
          assert_equal ([], []) (Lists.split 3 []))
    ; "split 3 on a 1 elem list" >:: (fun _ ->
          assert_equal ([1], []) (Lists.split 3 [1]))
    ; "split 3 on a 3 elem list" >:: (fun _ ->
          assert_equal ([1; 2; 3], []) (Lists.split 3 [1; 2; 3]))
    ; "split 3 on a 6 elem list" >:: (fun _ ->
          assert_equal ([1; 2; 3], [4; 5; 6]) (Lists.split 3 [1; 2; 3; 4; 5; 6]))
    ; "split 3 on a more complex example" >:: (fun _ ->
          assert_equal
            (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
            (Lists.split 3 ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"]))
    ; "split 5 on a more complex example" >:: (fun _ ->
          assert_equal
            (["a"; "b"; "c"; "d"], [])
            (Lists.split 5 ["a"; "b"; "c"; "d"]))
    ]
  )

let () = run_test_tt_main (
    "Lists: Problem 18" >:::
    [ "slice 0 2 on an empty list" >:: (fun _ ->
        assert_equal [] (Lists.slice 0 3 []))
    ; "slice 0 2 on a 5 elem list (first 3)" >:: (fun _ ->
          assert_equal [1; 2; 3] (Lists.slice 0 2 [1; 2; 3; 4; 5]))
    ; "slice 2 4 on a 6 elem list (middle elems)" >:: (fun _ ->
          assert_equal [3; 4; 5] (Lists.slice 2 4 [1; 2; 3; 4; 5; 6]))
    ; "slice 2 4 on a 3 elem list (end out of bounds)" >:: (fun _ ->
          assert_equal [3] (Lists.slice 2 4 [1; 2; 3]))
    ; "slice on a more complex example" >:: (fun _ ->
          assert_equal
            ["c"; "d"; "e"; "f"; "g"]
            (Lists.slice 2 6 ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"]))
    ]
  )

let () = run_test_tt_main (
    "Lists: Problem 19" >:::
    [ "rotate 2 on an empty list" >:: (fun _ ->
          assert_equal [] (Lists.rotate 2 []))
    ; "rotate 2 on a 1 elem list" >:: (fun _ ->
          assert_equal [1] (Lists.rotate 2 [1]))
    ; "rotate on a more complex example (positive)" >:: (fun _ ->
          assert_equal
            ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
            (Lists.rotate 3 ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"]))
    ; "rotate on a more complex example (negative)" >:: (fun _ ->
          assert_equal
            ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
            (Lists.rotate (-2) ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"]))
    ]
  )

let () = run_test_tt_main (
    "Lists: Problem 20" >:::
    [ "remove_at 3 on an empty list" >:: (fun _ ->
          assert_equal [] (Lists.remove_at 3 []))
    ; "remove_at 3 on a 1 elem list (out of bounds)" >:: (fun _ ->
          assert_equal [1] (Lists.remove_at 3 [1]))
    ; "remove_at 3 on a 6 elem list" >:: (fun _ ->
          assert_equal [1; 2; 3; 5; 6] (Lists.remove_at 3 [1; 2; 3; 4; 5; 6]))
    ; "remove_at on a more complex example" >:: (fun _ ->
          assert_equal
            ["a"; "c"; "d"]
            (Lists.remove_at 1 ["a"; "b"; "c"; "d"]))
    ]
  )

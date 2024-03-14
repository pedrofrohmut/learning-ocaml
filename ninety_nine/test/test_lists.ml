open OUnit2
open Ninety_nine

let suite1 =
  "Lists Problem 01" >:::
  [ "last on an empty list" >:: (fun _ -> assert_equal None (Lists.last []))
  ; "last on 1 elem list" >:: (fun _ -> assert_equal (Some 1) (Lists.last [1]))
  ; "last on 5 elem list" >:: (fun _ -> assert_equal (Some 5) (Lists.last [1; 2; 3; 4; 5]))
  ]

let suite2 =
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

let suite3 =
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

let suite4 =
  "Lists: Problem 04" >:::
  [ "length on an empty list" >:: (fun _ ->
        assert_equal 0 (Lists.length []))
  ; "length on a 1 elem list" >:: (fun _ ->
        assert_equal 1 (Lists.length [1]))
  ;  "length on a 5 elem list" >:: (fun _ ->
        assert_equal 5 (Lists.length [1; 2; 3; 4; 5]))
  ]

let suite5 =
  "Lists: Problem 05" >:::
  [ "rev on an empty list" >:: (fun _ ->
        assert_equal [] (Lists.rev []))
  ; "rev on 1 elem list" >:: (fun _ ->
        assert_equal [1] (Lists.rev [1]))
  ; "rev on 5 elem list" >:: (fun _ ->
        assert_equal [5; 4; 3; 2; 1] (Lists.rev [1; 2; 3; 4; 5]))
  ]

let suite6 =
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

let suite7 =
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

let suite8 =
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

let suite9 =
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

let suite10 =
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

let suite11 =
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

let suite12 =
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

let suite13 =
  let open Lists in
  "Lists: Problem13" >:::
  [ "encode3 on an empty list" >:: (fun _ ->
        assert_equal [] (Lists.encode3 []))
  ; "encode3 on 1 elem list" >:: (fun _ ->
        assert_equal [EncOne 1] (Lists.encode3 [1]))
  ; "encode3 on 3 elem list no repeated values" >:: (fun _ ->
        assert_equal [EncOne 1; EncOne 2; EncOne 3] (Lists.encode3 [1; 2; 3]))
  ; "encode3 on 5 elem list with 2 diff values but they repeat" >:: (fun _ ->
        assert_equal [EncMany (3, "a"); EncMany (2, "b")] (Lists.encode3 ["a"; "a"; "a"; "b"; "b"]))
  ; "encode3 on a more complex example" >:: (fun _ ->
        assert_equal
          [EncMany (4, "a"); EncOne "b"; EncMany (2, "c"); EncMany (2, "a"); EncOne "d"; EncMany (4, "e")]
          (Lists.encode3 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]))
  ]


let suite14 =
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

let suite15 =
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

let suite16 =
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

let suite17 =
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

let suite18 =
  "Lists: Problem 18" >:::
  [ "slice 0 2 on an empty list (error list is empty)" >:: (fun _ ->
        assert_raises (Failure "List is empty") (fun _ -> Lists.slice 0 2 []))
  ; "slice 0 2 on a 3 elem list (slice is all the list)" >:: (fun _ ->
        assert_equal [1; 2; 3] (Lists.slice 0 2 [1; 2; 3]))
  ; "slice 0 2 on a 5 elem list (slice the beggining)" >:: (fun _ ->
        assert_equal [1; 2; 3] (Lists.slice 0 2 [1; 2; 3; 4; 5]))
  ; "slice 2 4 on a 5 elem list (slice the end)" >:: (fun _ ->
        assert_equal [3; 4; 5] (Lists.slice 2 4 [1; 2; 3; 4; 5]))
  ; "slice 2 4 on a 6 elem list (middle elems)" >:: (fun _ ->
        assert_equal [3; 4; 5] (Lists.slice 2 4 [1; 2; 3; 4; 5; 6]))
  ; "slice 2 4 on a 3 elem list (last out of bounds)" >:: (fun _ ->
        assert_raises
          (Failure "Last position is out of bounds")
          (fun _ -> Lists.slice 2 4 [1; 2; 3]))
  ; "slice on a more complex example" >:: (fun _ ->
        assert_equal
          ["c"; "d"; "e"; "f"; "g"]
          (Lists.slice 2 6 ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"]))
  ; "slice -1 2 on a 5 elem list (first negative)" >:: (fun _ ->
        assert_raises
          (Failure "First position must be bigger than zero")
          (fun _ -> Lists.slice (-1) 2 [1; 2; 3; 4; 5]))
  ; "slice 0 -2 on a 5 elem list (last negative)" >:: (fun _ ->
        assert_raises
          (Failure "Last position must be bigger than zero")
          (fun _ -> Lists.slice 0 (-2) [1; 2; 3; 4; 5]))
  ; "slice 3 2 on a 5 elem list (first bigger than last)" >:: (fun _ ->
        assert_raises
          (Failure "First position cannot be bigger than last position")
          (fun _ -> Lists.slice 3 2 [1; 2; 3; 4; 5]))
  ]

let suite19 =
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

let suite20 =
  "Lists: Problem 20" >:::
  [ "remove_at 5 on an 3 elem list" >:: (fun _ ->
        assert_raises (Failure "Position out of bounds") (fun _ -> Lists.remove_at 5 [1; 2; 3]))
  ; "remove_at 3 on an empty list" >:: (fun _ ->
        assert_raises (Failure "List is empty") (fun _ -> Lists.remove_at 3 []))
  ; "remove_at (-3) on a 5 elem list" >:: (fun _ ->
        assert_raises (Failure "Position negative") (fun _ -> Lists.remove_at (-3) [1; 2; 3; 4; 5]))
  ; "remove_at 3 on a 6 elem list" >:: (fun _ ->
        assert_equal [1; 2; 3; 5; 6] (Lists.remove_at 3 [1; 2; 3; 4; 5; 6]))
  ; "remove_at on a more complex example" >:: (fun _ ->
        assert_equal
          ["a"; "c"; "d"]
          (Lists.remove_at 1 ["a"; "b"; "c"; "d"]))
  ]

let suite21 =
  let list4 = [1; 2; 3; 4] in
  "Lists: Problem 21" >:::
  [ "insert_at 0 666 on an empty list (init the list)" >:: (fun _ ->
        assert_equal [666] (Lists.insert_at 0 666 []))
  ; "insert_at 3 666 on a 1 elem list" >:: (fun _ ->
        assert_raises (Failure "Insert position out of bounds") (fun _ -> Lists.insert_at 3 666 [1]))
  ; "insert_at 3 666 on a 4 elem list" >:: (fun _ ->
        assert_equal [1; 2; 3; 666; 4] (Lists.insert_at 3 666 list4))
  ; "insert_at 4 666 on a 4 elem list (insert_at used as append)" >:: (fun _ ->
        assert_equal (list4 @ [666]) (Lists.insert_at 4 666 list4))
  ; "insert_at 0 666 on a 4 elem list (insert_at used as prepend)" >:: (fun _ ->
        assert_equal (666 :: list4) (Lists.insert_at 0 666 list4))
  ; "insert_at (-1) 666 on a 4 elem list (exception case negative position)" >:: (fun _ ->
        assert_raises
          (Failure "Negative value provided as a list position")
          (fun _ -> Lists.insert_at (-1) 666 list4))
  ]

let suite22 =
  "Lists: Problem 22" >:::
  [ "range 4 9" >:: (fun _ ->
        assert_equal [4; 5; 6; 7; 8; 9] (Lists.range 4 9))
  (* ; "range 9 4" >:: (fun _ -> *)
  (*       assert_equal [9; 8; 7; 6; 5; 4] (Lists.range 9 4)) *)
  ]

let suite23 =
  let seed = 1 in
  let list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 0] in
  (* With 'Random.init 1' the first results for
     Random.int 10 are: 4, 4, 2, 1, 8, 4, 1, 6, 2, 1 *)
  (* All the test here are base on the Random.init 1 *)
  "Lists: Problem 23" >:::
  [ "rand_select 1 on a 10 elem list" >:: (fun _ ->
        assert_equal [5] (Lists.rand_select seed 1 list))
  ; "rand_select 3 on a 10 elem list" >:: (fun _ ->
        assert_equal [5; 6; 9] (Lists.rand_select seed 3 list))
  ; "rand_select 5 on a 10 elem list" >:: (fun _ ->
        assert_equal [5; 6; 9; 0; 3] (Lists.rand_select seed 5 list))
  ; "rand_select 9 on a 10 elem list" >:: (fun _ ->
        assert_equal [5; 6; 9; 0; 3; 8; 7; 4; 1] (Lists.rand_select seed 9 list))
  ; "rand_select 10 on a 10 elem list" >:: (fun _ ->
        assert_equal [5; 6; 9; 0; 3; 8; 7; 4; 1; 2] (Lists.rand_select seed 10 list))
  ; "rand_select 11 on a 10 elem list" >:: (fun _ ->
        assert_raises
          (Failure "Invalid amount. Amount should be a integer from 0 to list length - 1")
          (fun () -> Lists.rand_select seed 11 list))
  ]

let suite24 =
  let seed = 1 in
  let limit = 10 in
  (* With 'Random.init 1' the first results for
     Random.int 10 are: 4, 4, 2, 1, 8, 4, 1, 6, 2, 1
     Unique list is [4; 2; 1; 8; 6; 9; 7; 3; 5; 0] *)
  (* All the test here are base on the Random.init 1 *)
  "Lists: Problem 24" >:::
  [ "lotto seed 1 10" >:: (fun _ ->
        assert_equal [4] (Lists.lotto seed 1 limit))
  ; "lotto seed 3 10" >:: (fun _ ->
        assert_equal [4; 2; 1] (Lists.lotto seed 3 limit))
  ; "lotto seed 5 10" >:: (fun _ ->
        assert_equal [4; 2; 1; 8; 6] (Lists.lotto seed 5 limit))
  ; "lotto seed 10 10" >:: (fun _ ->
        assert_equal [4; 2; 1; 8; 6; 9; 7; 3; 5; 0] (Lists.lotto seed 10 limit))
  ; "lotto seed 11 10" >:: (fun _ ->
        assert_raises (Failure "Amount out of reach") (fun _ -> Lists.lotto seed 11 limit))
  ; "lotto seed (-1) 10" >:: (fun _ ->
        assert_raises (Failure "Amount is negative") (fun _ -> Lists.lotto seed (-1) limit))
  ]

(* This is random an hard to test. Fewer tests here *)
let suite25 =
  let seed = 1 in
  "Lists: Problem 25" >:::
  [ "permutation on an empty list" >:: (fun _ ->
        assert_equal [] (Lists.permutation seed []))
  ; "permutation on 1 elem list" >:: (fun _ ->
        assert_equal [1] (Lists.permutation seed [1]))
  ; "permutation on 6 elem list" >:: (fun _ ->
        (* Random order for seed == 1 is: 2, 4, 2, 2, 0, 0 *)
        assert_equal
          ["c"; "f"; "d"; "e"; "a"; "b"]
          (Lists.permutation seed ["a"; "b"; "c"; "d"; "e"; "f"]))
  ]

let suite26 =
  "Lists: Problem 26" >:::
  [ "extract 1 on a 3 elem list" >:: (fun _ ->
        assert_equal [[1]; [2]; [3]] (Lists.extract 1 [1; 2; 3]))
  ; "extract 2 on a 3 elem list" >:: (fun _ ->
        assert_equal [[1; 2]; [1; 3]; [2; 3]] (Lists.extract 2 [1; 2; 3]))
  ; "extract 2 on 4 elem list" >:: (fun _ ->
        assert_equal [[1; 2]; [1; 3]; [1; 4]; [2; 3]; [2; 4]; [3; 4]] (Lists.extract 2 [1; 2; 3; 4]))
  ; "extract 2 on a 5 elem list" >:: (fun _ ->
        assert_equal
          [[1; 2]; [1; 3]; [1; 4]; [1; 5]; [2; 3]; [2; 4]; [2; 5]; [3; 4]; [3; 5]; [4; 5]]
          (Lists.extract 2 [1; 2; 3; 4; 5]))
  ]

  (* OLDER *)
  (* [ "extract 1 on an empty list" >:: (fun _ -> *)
  (*       assert_raises (Failure "List is empty") (fun _ -> Lists.extract 1 [])) *)
  (* ; "extract 0 on a 3 elem list" >:: (fun _ -> *)
  (*       assert_raises (Failure "Cannot extract less than 1 from list") (fun _ -> Lists.extract 0 [1; 2; 3])) *)
  (* ; "extract 4 on a 3 elem list" >:: (fun _ -> *)
  (*       assert_raises (Failure "N cannot be bigger than the length of the list") (fun _ -> Lists.extract 4 [1; 2; 3])) *)
  (* ; "extract 1 on a 3 elem list" >:: (fun _ -> *)
  (*       assert_equal [[1]; [2]; [3]] (Lists.extract 1 [1; 2; 3])) *)
  (* ; "extract 2 on a 3 elem list" >:: (fun _ -> *)
  (*       assert_equal [[1; 2]; [1; 3]; [2; 3]] (Lists.extract 2 [1; 2; 3])) *)
  (* ] *)


let () =
  let test_all = false in (* Flag to test all or a single test easy to change *)
  if not test_all then
    let single_suite = suite26 in
    run_test_tt_main single_suite
  else
    let rec main_run suites =
      match suites with
      | [] -> ()
      | x :: xs ->
        run_test_tt_main x;
        main_run xs
    in
    let suites =
      [ suite1; suite2; suite3; suite4; suite5; suite6; suite7; suite8; suite9; suite10; suite11
      ; suite12; suite13; suite14; suite15; suite16; suite17; suite18; suite19; suite20; suite21
      ; suite22; suite23; suite24; suite25
      ]
    in
    main_run suites

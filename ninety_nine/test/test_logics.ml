open OUnit2
open Ninety_nine

let suite46 =
  "Logic: Problem 46" >:::
    [ "example test" >:: (fun _ ->
        assert_equal
          [(true, true, true); (true, false, true); (false, true, false); (false, false, false)]
          (Logics.truth_table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")))))
    ]

let suite48 =
  let open Logics in
  "Logic: Problem 48" >:::
    [ "example test 2 symbols" >:: (fun _ ->
        assert_equal
          [([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
           ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)]
          (Logics.truth_table ["a"; "b"] (And (Var "a", Or (Var "a", Var "b")))))
    ; "example test 3 symbols" >:: (fun _ ->
          assert_equal
            [ ([("a", true); ("b", true); ("c", true)], true)
            ; ([("a", true); ("b", true); ("c", false)], true)
            ; ([("a", true); ("b", false); ("c", true)], true)
            ; ([("a", true); ("b", false); ("c", false)], false)
            ; ([("a", false); ("b", true); ("c", true)], false)
            ; ([("a", false); ("b", true); ("c", false)], false)
            ; ([("a", false); ("b", false); ("c", true)], false)
            ; ([("a", false); ("b", false); ("c", false)], false)
            ]
            (let a = Var "a" and b = Var "b" and c = Var "c" in
             Logics.truth_table ["a"; "b"; "c"] (Or (And (a, Or (b,c)), Or (And (a,b), And (a,c))))))
    ]

let () =
  let test_all = false in (* Flag to test all or a single test easy to change *)
  if not test_all then
    let single_suite = suite48 in
    run_test_tt_main single_suite
  else
    let rec main_run suites =
      match suites with
      | [] -> ()
      | x :: xs ->
         run_test_tt_main x;
         main_run xs
    in
    let suites = [suite46; suite48] in
    main_run suites

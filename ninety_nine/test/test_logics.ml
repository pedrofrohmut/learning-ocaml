open OUnit2
open Ninety_nine

let suite46 =
  "Logic: Problem 46" >:::
    [ "example test" >:: (fun _ ->
        assert_equal
          [(true, true, true); (true, false, true); (false, true, false); (false, false, false)]
          (Logics.truth_table "a" "b" (And (Var "a", Or (Var "a", Var "b")))))
    ]

let () =
  let test_all = false in (* Flag to test all or a single test easy to change *)
  if not test_all then
    let single_suite = suite46 in
    run_test_tt_main single_suite
  else
    let rec main_run suites =
      match suites with
      | [] -> ()
      | x :: xs ->
        run_test_tt_main x;
        main_run xs
    in
    let suites = [suite46] in
    main_run suites

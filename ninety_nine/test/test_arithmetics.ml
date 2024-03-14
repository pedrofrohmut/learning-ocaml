open OUnit2
open Ninety_nine

let suite31 =
  "Arithmetics: Problem 31" >:::
  [ "is_prime 1" >:: (fun _ ->
        assert_equal false (Arithmetics.is_prime 1))
  ; "is_prime 7" >:: (fun _ ->
        assert_equal true (Arithmetics.is_prime 7))
  ; "is_prime 12" >:: (fun _ ->
        assert_equal false (Arithmetics.is_prime 12))
  ]

let () =
  let test_all = false in (* Flag to test all or a single test easy to change *)
  if not test_all then
    let single_suite = suite31 in
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
      [ suite31
      ]
    in
    main_run suites

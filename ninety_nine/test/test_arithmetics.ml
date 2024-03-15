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

let suite32 =
  "Arithmetics: Problem 32" >:::
  [ "gcd 15 5" >:: (fun _ ->
        assert_equal 5 (Arithmetics.gcd 15 5))
  ; "gcd 252 105" >:: (fun _ ->
        assert_equal 21 (Arithmetics.gcd 252 105))
  ; "gcd 1386 3213" >:: (fun _ ->
        assert_equal 63 (Arithmetics.gcd 1386 3213))
  ]

let () =
  let test_all = false in (* Flag to test all or a single test easy to change *)
  if not test_all then
    let single_suite = suite32 in
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
      [ suite31; suite32
      ]
    in
    main_run suites

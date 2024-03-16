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
  [ "gcd 15 1" >:: (fun _ ->
        assert_equal 1 (Arithmetics.gcd 15 1))
  ; "gcd 15 5" >:: (fun _ ->
        assert_equal 5 (Arithmetics.gcd 15 5))
  ; "gcd 252 105" >:: (fun _ ->
        assert_equal 21 (Arithmetics.gcd 252 105))
  ; "gcd 1386 3213" >:: (fun _ ->
        assert_equal 63 (Arithmetics.gcd 1386 3213))
  ; "gcd 13 27" >:: (fun _ ->
        assert_equal 1 (Arithmetics.gcd 13 27))
  ; "gcd 20536 7826" >:: (fun _ ->
        assert_equal 2 (Arithmetics.gcd 20536 7826))
  ]

let suite33 =
  "Arithmetics: Problem 33" >:::
  [ "coprime 15 1" >:: (fun _ ->
        assert_equal true (Arithmetics.coprime 15 1))
  ; "coprime 15 5" >:: (fun _ ->
        assert_equal false (Arithmetics.coprime 15 5))
  ; "coprime 252 105" >:: (fun _ ->
        assert_equal false (Arithmetics.coprime 252 105))
  ; "coprime 1386 3213" >:: (fun _ ->
        assert_equal false (Arithmetics.coprime 1386 3213))
  ; "coprime 13 27" >:: (fun _ ->
        assert_equal true (Arithmetics.coprime 13 27))
  ; "coprime 20536 7826" >:: (fun _ ->
        assert_equal false (Arithmetics.coprime 20536 7826))
  ]

let () =
  let test_all = true in (* Flag to test all or a single test easy to change *)
  if not test_all then
    let single_suite = suite33 in
    run_test_tt_main single_suite
  else
    let rec main_run suites =
      match suites with
      | [] -> ()
      | x :: xs ->
        run_test_tt_main x;
        main_run xs
    in
    let suites = [suite31; suite32; suite33] in
    main_run suites

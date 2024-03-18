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

let suite34 =
  "Arithmetics: Problem 34" >:::
  [ "phi 10" >:: (fun _ ->
        assert_equal 4 (Arithmetics.phi 10))
  ; "phi 13" >:: (fun _ ->
        assert_equal 12 (Arithmetics.phi 13))
  ]

let suite35 =
  "Arithmetics: Problem 35" >:::
  [ "factors 1" >:: (fun _ ->
        assert_equal [] (Arithmetics.factors 1))
  ; "factors 25" >:: (fun _ ->
        assert_equal [5; 5] (Arithmetics.factors 25))
  ; "factors 60" >:: (fun _ ->
        assert_equal [2; 2; 3; 5] (Arithmetics.factors 60))
  ; "factors 100" >:: (fun _ ->
        assert_equal [2; 2; 5; 5] (Arithmetics.factors 100))
  ; "factors 315" >:: (fun _ ->
        assert_equal [3; 3; 5; 7] (Arithmetics.factors 315))
  ; "factors 1050" >:: (fun _ ->
        assert_equal [2; 3; 5; 5; 7] (Arithmetics.factors 1050))
  ; "factors 19110" >:: (fun _ ->
        assert_equal [2; 3; 5; 7; 7; 13] (Arithmetics.factors 19110))
  ]

let suite36 =
  "Arithmetics: Problem 36" >:::
  [ "factors_mult 1" >:: (fun _ ->
        assert_equal [] (Arithmetics.factors_mult 1))
  ; "factors_mult 25" >:: (fun _ ->
        assert_equal [(5, 2)] (Arithmetics.factors_mult 25))
  ; "factors_mult 60" >:: (fun _ ->
        assert_equal [(2, 2); (3, 1); (5, 1)] (Arithmetics.factors_mult 60))
  ; "factors_mult 100" >:: (fun _ ->
        assert_equal [(2, 2); (5, 2)] (Arithmetics.factors_mult 100))
  ; "factors_mult 315" >:: (fun _ ->
        assert_equal [(3, 2); (5, 1); (7, 1)] (Arithmetics.factors_mult 315))
  ; "factors_mult 1050" >:: (fun _ ->
        assert_equal [(2, 1); (3, 1); (5, 2); (7, 1)] (Arithmetics.factors_mult 1050))
  ; "factors_mult 19110" >:: (fun _ ->
        assert_equal [(2, 1); (3, 1); (5, 1); (7, 2); (13, 1)] (Arithmetics.factors_mult 19110))
  ]

let suite37 =
  "Arithmetics: Problem 37" >:::
  [ "phi_imp 10" >:: (fun _ ->
        assert_equal 4 (Arithmetics.phi_imp 10))
  ; "phi_imp 13" >:: (fun _ ->
        assert_equal 12 (Arithmetics.phi_imp 13))
  ]

(* Skipping the 38.
   There is no consistent result for that function manual testing will be enough *)

let suite39 =
  "Arithmetics: Problem 39" >:::
  [ "all_primes 1 10" >:: (fun _ ->
        assert_equal [2; 3; 5; 7] (Arithmetics.all_primes 1 10))
  ; "all_primes 10 20" >:: (fun _ ->
        assert_equal [11; 13; 17; 19] (Arithmetics.all_primes 10 20))
  ; "all_primes 100 120" >:: (fun _ ->
        assert_equal [101; 103; 107; 109; 113] (Arithmetics.all_primes 100 120))
  ]

let suite40 =
  "Arithmetics: Problem 40" >:::
  [ "goldbach 2" >:: (fun _ ->
        assert_raises
          (Failure "Only work on even numbers bigger than 2")
          (fun _ -> Arithmetics.goldbach 2))
  ; "goldbach 28" >:: (fun _ ->
        assert_equal (5, 23) (Arithmetics.goldbach 28))
  ; "goldbach 30" >:: (fun _ ->
        assert_equal (7, 23) (Arithmetics.goldbach 30))
  ; "goldbach 47" >:: (fun _ ->
        assert_raises
          (Failure "Only work on even numbers bigger than 2")
          (fun _ -> Arithmetics.goldbach 47))
  ; "goldbach2 2" >:: (fun _ ->
        assert_raises
          (Failure "Only work on even numbers bigger than 2")
          (fun _ -> Arithmetics.goldbach2 2))
  ; "goldbach2 28" >:: (fun _ ->
        assert_equal (5, 23) (Arithmetics.goldbach2 28))
  ; "goldbach2 30" >:: (fun _ ->
        assert_equal (7, 23) (Arithmetics.goldbach2 30))
  ; "goldbach2 47" >:: (fun _ ->
        assert_raises
          (Failure "Only work on even numbers bigger than 2")
          (fun _ -> Arithmetics.goldbach2 47))
  ]

let suite41 =
  "Arithmetics: Problem 41" >:::
  [ "goldbach_list 9 20" >:: (fun _ ->
        assert_equal
          [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13)); (20, (3, 17))]
          (Arithmetics.goldbach_list 9 20))
  ; "goldbach_limit 1 2000 50" >:: (fun _ ->
        assert_equal
          [(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789)); (1928, (61, 1867))]
          (Arithmetics.goldbach_limit 1 2000 50))
  ]

let () =
  let test_all = false in (* Flag to test all or a single test easy to change *)
  if not test_all then
    let single_suite = suite41 in
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
      [ suite31; suite32; suite33; suite34; suite35; suite36; suite37; suite39
      ; suite40; suite41
      ]
    in
    main_run suites

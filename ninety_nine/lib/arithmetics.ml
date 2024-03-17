(* Arithmetics *)

(*
    31. Determine whether a given integer number is prime. (medium)

    # is_prime 1;;
    - : bool = false

    # is_prime 7;;
    - : bool = true

    # is_prime 12;;
    - : bool = false
*)

let is_prime (n : int) : bool =
  let rec is_prime i n =
    match n mod i with
    | 0 when i == n -> true
    | 0 -> false
    | _ -> is_prime (i + 1) n
  in
  match n with 1 -> false | _ -> is_prime 2 n

(*
    32. Determine the greatest common divisor of two positive integer numbers. (medium)

    In mathematics, the Euclidean algorithm, is an efficient method for computing
    the greatest common divisor (GCD) of two integers (numbers), the largest
    number that divides them both without a remainder.

    # gcd 13 27;;
    - : int = 1

    # gcd 20536 7826;;
    - : int = 2
*)

let gcd (n1 : int) (n2 : int) : int =
  let rec gcd i res n1 n2 =
    if i > n1 || i > n2 then
      res
    else if n1 mod i == 0 && n2 mod i == 0 then
      gcd (i + 1) i n1 n2
    else
      gcd (i + 1) res n1 n2
  in
  gcd 1 1 n1 n2

(*
    33. Determine whether two positive integer numbers are coprime. (easy)

    Two numbers are coprime if their greatest common divisor equals 1.

    # coprime 13 27;;
    - : bool = true

    # coprime 20536 7826;;
    - : bool = false
*)

let coprime (n1 : int) (n2 : int) : bool = gcd n1 n2 == 1

(*
    34. Calculate Euler's totient function φ(m). (medium)

    Euler's so-called totient function φ(m) is defined as the number of positive
    integers r (1 ≤ r < m) that are coprime to m. We let φ(1) = 1.

    Find out what the value of φ(m) is if m is a prime number. Euler's totient
    function plays an important role in one of the most widely used public key
    cryptography methods (RSA). In this exercise you should use the most
    primitive method to calculate this function (there are smarter ways that we
    shall discuss later).

    # phi 10;;
    - : int = 4

    # phi 13;;
    - : int = 12
*)

let phi (n : int) : int =
  let rec phi acc i n =
    if i == n then
      acc
    else
      let i' = i + 1 in
      let acc' = if coprime i n then acc + 1 else acc in
      phi acc' i' n
  in
  phi 0 1 n

(*
    35. Determine the prime factors of a given positive integer. (medium)

    Construct a flat list containing the prime factors in ascending order.

    # factors 315;;
    - : int list = [3; 3; 5; 7]
*)

let factors (n : int) : int list =
  let rec factors acc i n =
    if n == 1 then
      List.rev acc
    else if is_prime i && n mod i == 0 then
      factors (i :: acc) i (n / i)
    else
      factors acc (i + 1) n
  in
  factors [] 1 n

(*
    36. Determine the prime factors of a given positive integer (2). (medium)

    Construct a list containing the prime factors and their multiplicity. Hint:
    The problem is similar to problem Run-length encoding of a list (direct
    solution).

    # factors 315;;
    - : (int * int) list = [(3, 2); (5, 1); (7, 1)]
*)

let factors_mult (n : int) : (int * int) list =
  let expanded_factors = factors n in
  let rec to_mult acc i list =
    match list with
    | x :: [] -> List.rev @@ (x, i) :: acc
    | x :: y :: rest when x == y -> to_mult acc (i + 1) (y :: rest)
    | x :: y :: rest -> to_mult ((x, i) :: acc) 1 (y :: rest)
    | _ -> failwith "Not implemented"
  in
  to_mult [] 1 expanded_factors

(*
    37. Calculate Euler's totient function φ(m) (improved). (medium)

    See problem "Calculate Euler's totient function φ(m)" for the definition of
    Euler's totient function. If the list of the prime factors of a number m is
    known in the form of the previous problem then the function phi(m) can be
    efficiently calculated as follows: Let [(p1, m1); (p2, m2); (p3, m3); ...] be
    the list of prime factors (and their multiplicities) of a given number
    m. Then φ(m) can be calculated with the following formula:

    φ(m) = (p1 - 1) × p1m1 - 1 × (p2 - 1) × p2m2 - 1 × (p3 - 1) × p3m3 - 1 × ⋯

    # phi_improved 10;;
    - : int = 4

    # phi_improved 13;;
    - : int = 12
*)



(*
38. Compare the two methods of calculating Euler's totient function. (easy)

Use the solutions of problems "Calculate Euler's totient function φ(m)" and "Calculate Euler's totient function φ(m) (improved)" to compare the algorithms. Take the number of logical inferences as a measure for efficiency. Try to calculate φ(10090) as an example.

# timeit phi 10090;;
- : float = 0.242475032806396484
# timeit phi_improved 10090;;
- : float = 8.296966552734375e-05

39. A list of prime numbers. (easy)

Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

# List.length (all_primes 2 7920);;
- : int = 1000

40. Goldbach's conjecture. (medium)

Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers. Write a function to find the two prime numbers that sum up to a given even integer.

# goldbach 28;;
- : int * int = (5, 23)

41. A list of Goldbach compositions. (medium)

Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.

In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.

# goldbach_list 9 20;;
- : (int * (int * int)) list =
[(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
 (20, (3, 17))]
# goldbach_limit 1 2000 50;;
- : (int * (int * int)) list =
[(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789));
 (1928, (61, 1867))]

Logic and Codes

Let us define a small "language" for boolean expressions containing variables:

# type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;
type bool_expr =
    Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

A logical expression in two variables can then be written in prefix notation. For example, (a ∨ b) ∧ (a ∧ b) is written:

# And (Or (Var "a", Var "b"), And (Var "a", Var "b"));;
- : bool_expr = And (Or (Var "a", Var "b"), And (Var "a", Var "b"))
*)

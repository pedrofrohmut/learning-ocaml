(*
    31. Determine whether a given integer number is prime. (medium)

    # not (is_prime 1);;
    - : bool = true

    # is_prime 7;;
    - : bool = true

    # not (is_prime 12);;
    - : bool = true
*)

(*
    32. Determine the greatest common divisor of two positive integer numbers. (medium)

    Use Euclid's algorithm.

    # gcd 13 27;;
    - : int = 1

    # gcd 20536 7826;;
    - : int = 2
*)

(*
    33. Determine whether two positive integer numbers are coprime. (easy)

    Two numbers are coprime if their greatest common divisor equals 1.

    # coprime 13 27;;
    - : bool = true

    # not (coprime 20536 7826);;
    - : bool = true
*)

(*
    34. Calculate Euler's totient function \u03c6(m). (medium)

    Euler's so-called totient function \u03c6(m) is defined as the number of positive integers
    r (1 \u2264 r < m) that are coprime to m. We let \u03c6(1) = 1.

    Find out what the value of \u03c6(m) is if m is a prime number. Euler's totient function plays an
    important role in one of the most widely used public key cryptography methods (RSA). In this
    exercise you should use the most primitive method to calculate this function (there are smarter
    ways that we shall discuss later).

    # phi 10;;
    - : int = 4

    # phi 13;;
    - : int = 12
*)

(*
    35. Determine the prime factors of a given positive integer. (medium)

    Construct a flat list containing the prime factors in ascending order.

    # factors 315;;
    - : int list = [3; 3; 5; 7]
*)

(*
    36. Determine the prime factors of a given positive integer (2). (medium)

    Construct a list containing the prime factors and their multiplicity. Hint: The problem is
    similar to problem Run-length encoding of a list (direct solution).

    # factors 315;;
    - : (int * int) list = [(3, 2); (5, 1); (7, 1)]
*)

(*
    37. Calculate Euler's totient function \u03c6(m) (improved). (medium)

    See problem "Calculate Euler's totient function \u03c6(m)" for the definition of Euler's totient
    function. If the list of the prime factors of a number m is known in the form of the previous
    problem then the function phi(m) can be efficiently calculated as follows: Let [(p1, m1); (p2,
    m2); (p3, m3); ...] be the list of prime factors (and their multiplicities) of a given number
    m. Then \u03c6(m) can be calculated with the following formula:

    \u03c6(m) = (p1 - 1) × p1m1 - 1 × (p2 - 1) × p2m2 - 1 × (p3 - 1) × p3m3 - 1 × \u22ef

    # phi_improved 10;;
    - : int = 4

    # phi_improved 13;;
    - : int = 12
*)

(*
    38. Compare the two methods of calculating Euler's totient function. (easy)

    Use the solutions of problems "Calculate Euler's totient function \u03c6(m)" and "Calculate
    Euler's totient function \u03c6(m) (improved)" to compare the algorithms. Take the number of
    logical inferences as a measure for efficiency. Try to calculate \u03c6(10090) as an example.

    # timeit phi 10090;;
    - : float = 0.242475032806396484

    # timeit phi_improved 10090;;
    - : float = 8.296966552734375e-05
*)

(*
    39. A list of prime numbers. (easy)

    Given a range of integers by its lower and upper limit, construct a list of all prime numbers in
    that range.

    # List.length (all_primes 2 7920);;
    - : int = 1000
*)

(*
    40. Goldbach's conjecture. (medium)

    Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime
    numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not
    been proved to be correct in the general case. It has been numerically confirmed up to very large
    numbers. Write a function to find the two prime numbers that sum up to a given even integer.

    # goldbach 28;;
    - : int * int = (5, 23)
*)

(*
    41. A list of Goldbach compositions. (medium)

    Given a range of integers by its lower and upper limit, print a list of all even numbers and
    their Goldbach composition.

    In most cases, if an even number is written as the sum of two prime numbers, one of them is very
    small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases
    there are in the range 2..3000.

    # goldbach_list 9 20;;
    - : (int * (int * int)) list =
    [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
    (20, (3, 17))]

    # goldbach_limit 1 2000 50;;
    - : (int * (int * int)) list =
    [(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789));
    (1928, (61, 1867))]
*)

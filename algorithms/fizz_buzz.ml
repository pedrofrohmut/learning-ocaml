(*
 fizz_buzz.ml

 Pedro Frohmut 2026 Copyrights

 Apply the fizz buzz algorithm to a sequence up to a limit number.
 Numbers that are divisible by 3 and not by 5 are Fizz
 Numbers that are divisible by 5 and not by 3 are Buzz
 Numbers that are divisible by 3 and 5 are FizzBuzz
 The valid limit is an integer positive number
*)

let fizz_buzz (limit : int) : string list =
  let rec aux (i : int) (n : int) : string list =
    if i > n then
      []
    else
      let curr = match i with
        | i when (i mod 15) == 0 -> "FizzBuzz"
        | i when (i mod 5)  == 0 -> "Buzz"
        | i when (i mod 3)  == 0 -> "Fizz"
        | _                      -> string_of_int i
      in
      curr :: aux (i + 1) n
  in
  aux 1 limit

let () =
  let fizz_buzzed = fizz_buzz 100 in
  Printf.printf "[%s]" (String.concat ", " fizz_buzzed);

(*
 fizz_buzz.ml

 Pedro Frohmut 2026 Copyrights

 Apply the fizz buzz algorithm to a sequence up to a limit number.
 Numbers that are divisible by 3 and not by 5 are Fizz
 Numbers that are divisible by 5 and not by 3 are Buzz
 Numbers that are divisible by 3 and 5 are FizzBuzz
 The valid limit is an integer positive number
*)

let fizz_buzz (limit: int): string list =
  let rec aux (i: int) (n: int): string list =
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


let fizz_buzz_str (i: int): string =
  match i with
  | i when (i mod 15) == 0 -> "FizzBuzz"
  | i when (i mod 5)  == 0 -> "Buzz"
  | i when (i mod 3)  == 0 -> "Fizz"
  | _                      -> string_of_int i


let rec fizz_buzz_aux (i: int) (n: int): string list =
  if i > n then
    []
  else
    fizz_buzz_str i :: fizz_buzz_aux (i + 1) n

(* Broken in smaller functions *)
let fizz_buzz2 (limit: int): string list =
  fizz_buzz_aux 1 limit


(* With optional arguments so you dont need the auxiliar function just to have
   an extra argument *)
let rec fizz_buzz3 ?(n=1) (limit) =
  if n > limit then
    []
  else
    fizz_buzz_str n :: fizz_buzz3 ~n:(n + 1) limit


let fizz_buzz4 limit =
  (* No accumulator, no auxiliar counter and prepending the order will be reversed *)
  let rec get_numbers ?(acc=[]) n =
    if n = 0 then
      acc
    else
      get_numbers ~acc:(n :: acc) (n - 1)
  in
  (* Returning a string for the cases of fizz buzz, only converts otherwise *)
  let fizz_buzz n =
    match n with
    | n when n mod 15 = 0 -> "FizzBuzz"
    | n when n mod 5 = 0 -> "Buzz"
    | n when n mod 3 = 0 -> "Fizz"
    | _ -> string_of_int n
  in
  let numbers = get_numbers limit in
  List.map (fun n -> fizz_buzz n) numbers


let main (): unit =
  let fizz_buzzed = fizz_buzz 100 in
  Printf.printf "Fizz_Buzz: [%s]\n" (String.concat ", " fizz_buzzed);

  let fizz_buzzed2 = fizz_buzz2 100 in
  Printf.printf "Fizz_Buzz2: [%s]\n" (String.concat ", " fizz_buzzed2);

  let fizz_buzzed3 = fizz_buzz3 100 in
  Printf.printf "Fizz_Buzz3: [%s]\n" (String.concat ", " fizz_buzzed3);

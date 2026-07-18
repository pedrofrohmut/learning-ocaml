(*
  prime-numbers.ml
  Pedro Frohmut 2026 Copyrights

  Get all the prime numbers from 2 to a limit
 *)

let dec (n: int): int = n - 1


let inc (n: int): int = n + 1


let is_prime (n: int): bool =
  let rec aux (x: int) (i: int) =
    if i = n then
      true
    else if (x mod i) = 0 then
      false
    else
      aux n (inc i)
  in
  aux n 2


let rec list_primes (n: int): int list =
  let rec aux (n: int) (acc: int list): int list =
    if n = 1 then
      acc
    else
      let new_acc = if is_prime n then (n :: acc) else acc in
      aux (dec n) new_acc
  in
  aux n []

(* With optional argument you dont need the aux function *)
let rec primes ?(acc: int list = []) (n: int): int list =
  if n = 1 then
    acc
  else
    let new_acc = if is_prime n then (n :: acc) else acc in
    primes ~acc:new_acc (dec n) (* You need to label optional arguments *)


let () =
  let lst = list_primes 100 in
  let str_lst = List.map string_of_int lst in
  Printf.printf "List Primes: [%s]\n" (String.concat ", " str_lst);

  let primes = List.map string_of_int (primes 100) in
  Printf.printf "Primes:      [%s]\n" (String.concat ", " primes)

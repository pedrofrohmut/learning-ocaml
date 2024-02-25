(*
  1. Write a function last : 'a list -> 'a option that returns the last element
  of a list. (easy)
*)

let rec my_last (xs : 'a list) : 'a option =
  match xs with
  | [] -> None
  | [x] -> Some x
  | _ :: xs -> my_last xs

(*
  2. Find the last but one (last and penultimate) elements of a list. (easy)
*)

let rec my_penultimate (xs : 'a list) : 'a option =
  match xs with
  | [] -> None
  | [_] -> None
  | x :: [_] -> Some x
  | _ :: xs -> my_penultimate xs

(*
  3. Find the K'th element of a list. (easy)
*)

let rec my_at (xs: 'a list) (i: int) : 'a option =
  match xs with
  | [] -> None
  | x :: xs -> if i = 0 then (Some x) else my_at xs (i - 1)

(*
  4. Find the number of elements of a list. (easy)
*)

let my_length (xs: 'a list) : int =
  let rec len (ys : 'a list) (i : int) : int =
    match ys with
    | [] -> i
    | _ :: ys -> len ys (i + 1)
  in
    len xs 0

(*
  5. Reverse a list. (easy)
*)

let rec my_reverse xs =
  let rec _last ys =
    match ys with
    | [] -> None
    | [y] -> (Some y)
    | _ :: ys -> _last ys
  in

  let rec _without_last ys =
    match ys with
    | [] -> []
    | [_] -> []
    | y :: ys -> y :: _without_last ys
  in

  match xs with
  | [] -> []
  | [x] -> [x]
  | x :: xs ->
    match _last xs with
    | None -> []
    | (Some y) -> y :: my_reverse (_without_last (x :: xs))

(*
  6. Find out whether a list is a palindrome. (easy)
*)

let rec my_is_palindrome xs =
  let rec _last ys =
    match ys with
    | [] -> None
    | [y] -> (Some y)
    | _ :: ys -> _last ys
  in

  let rec _without_last ys =
    match ys with
    | [] -> []
    | [_] -> []
    | y :: ys -> y :: _without_last ys
  in

  match xs with
  | [] -> true
  | [_] -> true
  | y :: ys ->
    match (_last ys) with
    | None -> false
    | (Some z) -> if z = y then my_is_palindrome (_without_last ys) else false

(* 7. Flatten a nested list structure. (medium) *)

(* 8. Eliminate consecutive duplicates of list elements. (medium) *)

(* 9. Pack consecutive duplicates of list elements into sublists. (medium) *)

(* 10. Run-length encoding of a list. (easy) *)

(* 10. Run-length encoding of a list. (easy) *)

(* 12. Decode a run-length encoded list. (medium) *)

(* 13. Run-length encoding of a list (direct solution). (medium) *)

(* 14. Duplicate the elements of a list. (easy) *)

(* 15. Replicate the elements of a list a given number of times. (medium) *)

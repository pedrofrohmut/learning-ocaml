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

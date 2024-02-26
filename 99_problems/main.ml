(*
  1. Write a function last : 'a list -> 'a option that returns the last element
  of a list. (easy)

  # last ["a" ; "b" ; "c" ; "d"];;
  - : string option = Some "d"
  # last [];;
  - : 'a option = None
*)

let rec my_last (xs : 'a list) : 'a option =
  match xs with
  | [] -> None
  | [x] -> Some x
  | _ :: xs -> my_last xs

(*
  2. Find the last but one (last and penultimate) elements of a list. (easy)

  # last_two ["a"; "b"; "c"; "d"];;
  - : (string * string) option = Some ("c", "d")
  # last_two ["a"];;
  - : (string * string) option = None
*)

let rec my_penultimate (xs : 'a list) : 'a option =
  match xs with
  | [] -> None
  | [_] -> None
  | x :: [_] -> Some x
  | _ :: xs -> my_penultimate xs

(*
  3. Find the K'th element of a list. (easy)

  # at 3 ["a"; "b"; "c"; "d"; "e"];;
  - : string option = Some "c"
  # at 3 ["a"];;
  - : string option = None

  REMARK: OCaml has List.nth which numbers elements from 0 and raises an exception
  if the index is out of bounds.

  # List.nth ["a"; "b"; "c"; "d"; "e"] 2;;
  - : string = "c"
  # List.nth ["a"] 2;;
  Exception: Failure "nth".
*)

let rec my_at (xs: 'a list) (i: int) : 'a option =
  match xs with
  | [] -> None
  | x :: xs -> if i = 0 then (Some x) else my_at xs (i - 1)

(*
  4. Find the number of elements of a list. (easy)

  OCaml standard library has List.length but we ask that you reimplement it. Bonus
  for a tail recursive solution.

  # length ["a"; "b"; "c"];;
  - : int = 3
  # length [];;
  - : int = 0
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

  OCaml standard library has List.rev but we ask that you reimplement it.

  # rev ["a"; "b"; "c"];;
  - : string list = ["c"; "b"; "a"]
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

  HINT: a palindrome is its own reverse.

  # is_palindrome ["x"; "a"; "m"; "a"; "x"];;
  - : bool = true
  # not (is_palindrome ["a"; "b"]);;
  - : bool = true
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

(*
  7. Flatten a nested list structure. (medium)

  # (* There is no nested list type in OCaml, so we need to define one
       first. A node of a nested list is either an element, or a list of
       nodes. *)
    type 'a node =
      | One of 'a
      | Many of 'a node list;;
  type 'a node = One of 'a | Many of 'a node list

  Case 2:
  # flatten [Many [One 1; One 2; One 3]]
  - : int list = [1; 2; 3]

  Case 3:
  # flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;
  - : string list = ["a"; "b"; "c"; "d"; "e"]
*)

type 'a node =
  | One of 'a
  | Many of 'a node list

let my_flatten my_list =

  let rec _append list1 list2 =
    match list1 with
    | [] -> list2
    | x :: xs -> x :: _append xs list2
  in

  let rec _flatten result list =
    match list with
    | [] -> result
    | x :: xs -> match x with
      | (One y)   -> _flatten (_append result [y]) xs
      | (Many ys) -> _flatten (_flatten result ys) xs
  in

  _flatten [] my_list

(* 8. Eliminate consecutive duplicates of list elements. (medium) *)

(* 9. Pack consecutive duplicates of list elements into sublists. (medium) *)

(* 10. Run-length encoding of a list. (easy) *)

(* 10. Run-length encoding of a list. (easy) *)

(* 12. Decode a run-length encoded list. (medium) *)

(* 13. Run-length encoding of a list (direct solution). (medium) *)

(* 14. Duplicate the elements of a list. (easy) *)

(* 15. Replicate the elements of a list a given number of times. (medium) *)

(*
  palindrome-check.ml
  Pedro Frohmut 2026 Copyrights

  Checks if a string is a palindrome.

  A palindrome is a word that can be read from right to left and also from left to
  right
*)


(* Possible to get head :: tail from string the same way you can do with lists.
   It uses option so there is no error with empty or single char strings *)
let head_tail_str (str: string): (char option * string option) =
  let len = String.length str in
  if len == 0 then
    None, None
  else if len == 1 then
    let head = String.get str 0 in
    Some head, None
  else
    let head = String.get str 0 in
    let tail = String.sub str 1 (len - 1) in
    Some head, Some tail


(* Uses the head_tail_str to reverse a string getting the head and prepending it
   to an accumulator *)
let rec reverse_str ?(acc: string = "") (str: string): string =
  let (head, tail) = head_tail_str str in
  match (head, tail) with
  | (None, _)        -> acc
  | (Some x, None)   -> reverse_str ~acc:((String.make 1 x) ^ acc) ""
  | (Some x, Some y) -> reverse_str ~acc:((String.make 1 x) ^ acc) y


(* just a function to return the passed value incremented *)
let inc (n: int): int = n + 1
let dec (n: int): int = n - 1


(* Reverse a list *)
let rec reverse_list ?(acc: 'a list = []) (lst: 'a list): 'a list =
  match lst with
  | [] -> acc
  | x :: xs -> reverse_list ~acc:(x :: acc) xs


(* Changes a string into a char list *)
let list_of_string (str_source: string): char list =
  let rec aux acc i src =
    let len = String.length src in
    if i == len then
      reverse_list acc
    else
      let x = String.get src i in
      aux (x :: acc) (inc i) src
  in
  aux [] 0 str_source


(* The most straigth forward approach. With a counter from 0 to length - 1 gets
   char by char and prepend it to an accumulator and returns the accumulator when
   the counter gets to the end *)
let reverse_str2 (str: string): string =
  let rec aux acc i str =
    let len = String.length str in
    if i = len then
      acc
    else
      let ch = String.get str i in
      let x = String.make 1 ch in
      aux (x ^ acc) (inc i) str
  in
  aux "" 0 str


(* Checks if a string is the same of this string reversed *)
let is_palindrome (word: string): bool =
  let reversed = reverse_str2 word in
  reversed = word


(* Checks char by char if they are the same.
   Matches left pos and right pos.
   No extra work not reversing lists *)
let is_palindrome2 (word: string): bool =
  let rec aux limit left right =
    let is_equal = String.get word left = String.get word right in
    if not is_equal then
      false
    else if left > limit then
      true
    else
      aux limit (inc left) (dec right)
  in

  let len = float_of_int (String.length word) in
  let limit = int_of_float (ceil (len /. 2.0)) in
  let left = 0 in
  let right = (String.length word) - 1 in

  aux limit left right


let () =
  let test (str: string): unit =
    let str_result = string_of_bool (is_palindrome2 str) in
    Printf.printf "Is %s a palindrome? %s\n" str str_result
  in
  test "foobar";
  test "racecar";
  test "carac";
  test "ababa";
  test "hello"

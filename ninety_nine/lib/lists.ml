(*
    1. Write a function last : 'a list -> 'a option that returns the last element
    of a list. (easy)

    # last ["a" ; "b" ; "c" ; "d"];;
    - : string option = Some "d"
    # last [];;
    - : 'a option = None
*)

let rec last (list : 'a list) : 'a option =
  match list with
  | [] -> None
  | x :: [] -> Some x
  | _ :: xs -> last xs

(*
    2. Find the last but one (last and penultimate) elements of a list. (easy)

    # last_two ["a"; "b"; "c"; "d"];;
    - : (string * string) option = Some ("c", "d")
    # last_two ["a"];;
    - : (string * string) option = None
*)

let rec last_two (list : 'a list) : ('a * 'a) option =
  match list with
  | [] -> None
  | _ :: [] -> None
  | x :: y :: [] -> Some (x, y)
  | _ :: xs -> last_two xs

(*
    3. Find the K'th element of a list. (easy)

    # at 3 ["a"; "b"; "c"; "d"; "e"];;
    - : string option = Some "c"
    # at 3 ["a"];;
    - : string option = None

    REMARK: OCaml has List.nth which numbers elements from 0 and raises an
    exception if the index is out of bounds.

    # List.nth ["a"; "b"; "c"; "d"; "e"] 2;;
    - : string = "c"
    # List.nth ["a"] 2;;
    Exception: Failure "nth".
*)

(* Raises exception if index out of bounds list passed  *)
let rec at (pos : int) (list : 'a list) : 'a =
  match list with
  | [] -> failwith "Index out of bounds"
  | x :: xs -> if pos > 0 then at (pos - 1) xs else x

(* Does not fail. Just returns None if index out of bounds *)
let rec at_opt (pos : int) (list : 'a list) : 'a option =
  match list with
  | [] -> None
  | x :: xs -> if pos > 0 then at_opt (pos - 1) xs else Some x

(*
    4. Find the number of elements of a list. (easy)

    OCaml standard library has List.length but we ask that you reimplement
    it. Bonus for a tail recursive solution.

    # length ["a"; "b"; "c"];;
    - : int = 3
    # length [];;
    - : int = 0
*)

let length (list: 'a list) : int =
  let rec aux n list' =
    match list' with
    | [] -> n
    | _ :: xs -> aux (n + 1) xs
  in
  aux 0 list

(*
    5. Reverse a list. (easy)

    OCaml standard library has List.rev but we ask that you reimplement it.

    # rev ["a"; "b"; "c"];;
    - : string list = ["c"; "b"; "a"]
*)

let rev (list : 'a list) : 'a list =
  let rec aux acc list' =
    match list' with
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
  in
  aux [] list


(*
    6. Find out whether a list is a palindrome. (easy)

    HINT: a palindrome is its own reverse.

    # is_palindrome ["x"; "a"; "m"; "a"; "x"];;
    - : bool = true
    # not (is_palindrome ["a"; "b"]);;
    - : bool = true
*)

let is_palindrome (list : 'a list) : bool = list = rev list

(*
    7. Flatten a nested list structure. (medium)

    # (* There is no nested list type in OCaml, so we need to define one
        first. A node of a nested list is either an element, or a list of
        nodes. *)
    type 'a node =
        | One of 'a
        | Many of 'a node list;;
    type 'a node = One of 'a | Many of 'a node list

    # flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;
    - : string list = ["a"; "b"; "c"; "d"; "e"]
*)

type 'a node = One of 'a | Many of 'a node list

let flatten (list : 'a node list) : 'a list =
  let rec aux acc list' =
    match list' with
    | [] -> acc
    | One value :: xs -> aux (value :: acc) xs
    | Many value :: xs -> aux (aux acc value) xs
  in
  list |> aux [] |> rev

(*
    8. Eliminate consecutive duplicates of list elements. (medium)

    # compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
    - : string list = ["a"; "b"; "c"; "a"; "d"; "e"]
*)

let compress (list : 'a list) : 'a list =
  let rec aux curr list' =
    match list' with
    | [] -> []
    | x :: xs -> if x == curr then aux curr xs else x :: aux x xs
  in
  match list with
  | [] -> []
  | x :: [] -> [x]
  | x :: xs -> x :: aux x xs

(*
    9. Pack consecutive duplicates of list elements into sublists. (medium)

    # pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;
    - : string list list =
    [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
    ["e"; "e"; "e"; "e"]]
*)

let pack (list : 'a list) : 'a list list =
  let rec aux acc curr list' =
    match list' with
    | [] -> acc :: []
    | x :: xs ->
        if x == curr
        then aux (x :: acc) curr xs
        else acc :: aux [x] x xs
  in
  match list with
  | [] -> []
  | x :: [] -> [[x]]
  | x :: xs -> aux [x] x xs

(*
    10. Run-length encoding of a list. (easy)

    If you need so, refresh your memory about run-length encoding.

    Here is an example:

    # encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
    - : (int * string) list =
    [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
*)

let encode (list : 'a list) : (int * 'a) list =
  let rec aux i curr list' =
    match list' with
    | [] -> (i, curr) :: []
    | x :: xs ->
      if x == curr
      then aux (i + 1) curr xs
      else (i, curr) :: aux 1 x xs
  in
  match list with
  | [] -> []
  | x :: xs -> aux 0 x (x :: xs)

(*
    11. Modified run-length encoding. (easy)

    Modify the result of the previous problem in such a way that if an element
    has no duplicates it is simply copied into the result list. Only elements
    with duplicates are transferred as (N E) lists.

    Since OCaml lists are homogeneous, one needs to define a type to hold both
    single elements and sub-lists.

    # type 'a rle =
    | One of 'a
    | Many of int * 'a;;
    type 'a rle = One of 'a | Many of int * 'a

    # encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
    - : string rle list =
    [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
    Many (4, "e")]
*)

type 'a encode_t = EncOne of 'a | EncMany of (int * 'a)

let encode2 (list : 'a list) : 'a encode_t list =
  let rec aux i curr list' =
    match list' with
    | [] ->
      let last_elem = if i == 1 then EncOne curr else EncMany (i, curr) in
      last_elem :: []
    | x :: xs ->
      if x == curr
        then aux (i + 1) curr xs
        else
          let new_elem = if i <= 1 then EncOne curr else EncMany (i, curr) in
          new_elem :: aux 1 x xs
  in
  match list with
  | [] -> []
  | x::xs -> aux 1 x xs

(*
    12. Decode a run-length encoded list. (medium)

    Given a run-length code list generated as specified in the previous problem,
    construct its uncompressed version.

    # decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
    - : string list =
    ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
*)

let rec decode (list: 'a encode_t list) : 'a list =
  let rec decode_elem elem =
    match elem with
    | EncOne x -> [x]
    | EncMany (0, _) -> []
    | EncMany (i, x) -> x :: decode_elem (EncMany ((i - 1), x))
  in
  match list with
  | [] -> []
  | x :: xs -> decode_elem x @ decode xs

(*
    13. Run-length encoding of a list (direct solution). (medium)

    Implement the so-called run-length encoding data compression method
    directly. I.e. don't explicitly create the sublists containing the
    duplicates, as in problem "Pack consecutive duplicates of list elements into
    sublists", but only count them. As in problem "Modified run-length encoding",
    simplify the result list by replacing the singleton lists (1 X) by X.

    # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
    - : string rle list =
    [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
    Many (4, "e")]
*)

(* Already did this in Problem 11. Just copying *)
let encode3 (list : 'a list) : 'a encode_t list =
  let rec aux i curr list' =
    match list' with
    | [] ->
      let last_elem = if i == 1 then EncOne curr else EncMany (i, curr) in
      last_elem :: []
    | x :: xs ->
      if x == curr
        then aux (i + 1) curr xs
        else
          let new_elem = if i <= 1 then EncOne curr else EncMany (i, curr) in
          new_elem :: aux 1 x xs
  in
  match list with
  | [] -> []
  | x::xs -> aux 1 x xs

(*
    14. Duplicate the elements of a list. (easy)

    # duplicate ["a"; "b"; "c"; "c"; "d"];;
    - : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
*)

let rec duplicate (list : 'a list) : 'a list =
  match list with
  | [] -> []
  | x :: xs -> x :: x :: duplicate xs


(*
    15. Replicate the elements of a list a given number of times. (medium)

    # replicate ["a"; "b"; "c"] 3;;
    - : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
*)

let rec replicate (n : int) (list : 'a list) : 'a list =
  let rec replicate_elem i elem =
    if i == 0 then [] else elem :: replicate_elem (i - 1) elem
  in
  match list with
  | [] -> []
  | x :: xs -> replicate_elem 3 x @ replicate n xs

(*
    16. Drop every N'th element from a list. (medium)

    # drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
    - : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
*)

let drop (n : int) (list : 'a list) : 'a list =
  let rec aux i n list =
    match list with
    | [] -> []
    | x :: xs -> if i == 1 then aux n n xs else x :: aux (i - 1) n xs
  in
  aux n n list

(*
    17. Split a list into two parts; the length of the first part is given. (easy)

    If the length of the first part is longer than the entire list, then the
    first part is the list and the second part is empty.

    # split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
    - : string list * string list = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])

    # split ["a"; "b"; "c"; "d"] 5;;
    - : string list * string list = (["a"; "b"; "c"; "d"], [])
*)

let split (n : int) (list: 'a list) : ('a list * 'a list) =
  let rec aux acc i list =
    match list with
    | [] -> (rev acc, [])
    | x :: xs -> if i > 0 then aux (x :: acc) (i - 1) xs else (rev acc, x :: xs)
  in
  aux [] n list

(*
    18. Extract a slice from a list. (medium)

    Given two indices, i and k, the slice is the list containing the elements
    between the i'th and k'th element of the original list (both limits
    included). Start counting the elements with 0 (this is the way the List
    module numbers elements).

    # slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;
    - : string list = ["c"; "d"; "e"; "f"; "g"]
*)

let slice (first : int) (last : int) (list : 'a list) : 'a list =
  let rec aux i first last list =
    match list with
    | (_ :: xs) when i < first -> aux (i + 1) first last xs
    | (x :: xs) when i <= last -> x :: aux (i + 1) first last xs
    | _ -> []
  in
  aux 0 first last list

(*
    19. Rotate a list N places to the left. (medium)

    # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
    - : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]

    # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
    - : string list = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
*)

let rotate (n : int) (list : 'a list) : 'a list =
  let split_pos = if n < 0 then (length list) + n else n in
  let rec aux acc i n list =
    match list with
    | [] -> acc
    | x :: xs when i < n -> aux (x :: acc) (i + 1) n xs
    | xs -> xs @ (rev acc)
  in
  aux [] 0 split_pos list

(*
    20. Remove the K'th element from a list. (easy)

    The first element of the list is numbered 0, the second 1,...

    # remove_at 1 ["a"; "b"; "c"; "d"];;
    - : string list = ["a"; "c"; "d"]
*)

let remove_at (n : int) (list : 'a list) : 'a list =
  let rec aux i n list =
    match list with
    | [] -> []
    | x :: xs when i < n -> x :: aux (i + 1) n xs
    | _ :: xs -> xs
  in
  aux 0 n list

(*
    21. Insert an element at a given position into a list. (easy)

    Start counting list elements with 0. If the position is larger or equal to
    the length of the list, insert the element at the end. (The behavior is
    unspecified if the position is negative.)

    # insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
    - : string list = ["a"; "alfa"; "b"; "c"; "d"]

    # insert_at "alfa" 3 ["a"; "b"; "c"; "d"];;
    - : string list = ["a"; "b"; "c"; "alfa"; "d"]

    # insert_at "alfa" 4 ["a"; "b"; "c"; "d"];;
    - : string list = ["a"; "b"; "c"; "d"; "alfa"]
*)

let insert_at (n : int) (elem: 'a) (list : 'a list) : 'a list =
  let rec aux i n elem list =
    match list with
    | [] -> []
    | x :: [] when i == (n - 1) -> x :: elem :: []
    | x :: [] when i == n -> elem :: x :: []
    | x :: xs when i == n -> elem :: x :: xs
    | x :: xs -> x :: aux (i + 1) n elem xs
  in
  aux 0 n elem list

(*
    22. Create a list containing all integers within a given range. (easy)

    If first argument is greater than second, produce a list in decreasing order.

    # range 4 9;;
    - : int list = [4; 5; 6; 7; 8; 9]

    # range 9 4;;
    - : int list = [9; 8; 7; 6; 5; 4]
*)

let rec range (first: int) (last: int) : 'a list =
  if first == last
    then last :: []
    else
      if first > last
        then first :: range (first - 1) last
        else first :: range (first + 1) last

(*
23. Extract a given number of randomly selected elements from a list. (medium)

The selected items shall be returned in a list. We use the Random module but do not initialize it with Random.self_init for reproducibility.

# rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
- : string list = ["g"; "d"; "a"]

24. Lotto: Draw N different random numbers from the set 1..M. (easy)

The selected numbers shall be returned in a list.

# lotto_select 6 49;;
- : int list = [10; 20; 44; 22; 41; 2]

25. Generate a random permutation of the elements of a list. (easy)

# permutation ["a"; "b"; "c"; "d"; "e"; "f"];;
- : string list = ["a"; "e"; "f"; "b"; "d"; "c"]

26. Generate the combinations of K distinct objects chosen from the N elements of a list. (medium)

In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

# extract 2 ["a"; "b"; "c"; "d"];;
- : string list list =
[["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]

27. Group the elements of a set into disjoint subsets. (medium)

    In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.
    Generalize the above function in a way that we can specify a list of group sizes and the function will return a list of groups.

# group ["a"; "b"; "c"; "d"] [2; 1];;
- : string list list list =
[[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
 [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
 [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
 [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]]

28. Sorting a list of lists according to length of sublists. (medium)

    We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

    Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.

# length_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
               ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;
- : string list list =
[["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; ["f"; "g"; "h"];
 ["i"; "j"; "k"; "l"]]
# frequency_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
                  ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;
- : string list list =
[["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["d"; "e"];
 ["d"; "e"]; ["m"; "n"]]

*)

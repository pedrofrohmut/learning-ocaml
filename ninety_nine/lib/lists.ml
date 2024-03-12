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

let rec at (pos : int) (list : 'a list) : 'a =
  match (pos, list) with
  | (_, []) -> failwith "Index out of bounds"
  | (0, x :: _ ) -> x
  | (_, _ :: xs) -> at (pos - 1) xs

let rec at_opt (pos : int) (list : 'a list) : 'a option =
  match (pos, list) with
  | (_, []) -> None
  | (0, x :: _) -> Some x
  | (_, _ :: xs) -> at_opt (pos - 1) xs

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
  let rec aux n list =
    match list with
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
  let rec aux acc list =
    match list with
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

    There is no nested list type in OCaml, so we need to define one first. A node
    of a nested list is either an element, or a list of nodes.

    type 'a node = One of 'a | Many of 'a node list

    # flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;
    - : string list = ["a"; "b"; "c"; "d"; "e"]
*)

type 'a node = One of 'a | Many of 'a node list

let flatten (list : 'a node list) : 'a list =
  let rec aux acc list =
    match list with
    | [] -> acc
    | One value :: xs -> aux (value :: acc) xs
    | Many value :: xs -> aux (aux acc value) xs
  in
  rev (aux [] list)

(*
    8. Eliminate consecutive duplicates of list elements. (medium)

    # compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
    - : string list = ["a"; "b"; "c"; "a"; "d"; "e"]
*)

(* With tail call optimization *)
let compress (list : 'a list) : 'a list =
  let rec aux acc list =
    match list with
    | [] -> []
    | x :: [] -> x :: []
    | x :: y :: [] ->
      if x == y then
        x :: acc
      else
        y :: x :: acc
    | x :: y :: xs ->
      if x == y then
        aux acc (y :: xs)
      else
        aux (x :: acc) (y :: xs)
  in
  rev @@ aux [] list

let compress_notco (list : 'a list) : 'a list =
  let rec aux curr list =
    match list with
    | [] -> []
    | x :: xs -> if x == curr then aux curr xs else x :: aux x xs
  in

  match list with
  | [] -> []
  | x :: [] -> x :: []
  | x :: xs -> x :: aux x xs

(*
    9. Pack consecutive duplicates of list elements into sublists. (medium)

    # pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;
    - : string list list =
    [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
    ["e"; "e"; "e"; "e"]]
*)

(* With TCO *)
let pack (list : 'a list) : 'a list list =
  let rec aux res acc list =
    match acc, list with
    | [], [] -> []
    | _, [] -> rev (acc :: res)
    | [], y :: ys -> aux res (y :: acc) ys
    | x :: _, y :: ys when x == y -> aux res (y :: acc) ys
    | _, y :: ys -> aux (acc :: res) [] (y :: ys)
  in
  aux [] [] list

let pack_notco (list : 'a list) : 'a list list =
  let rec aux acc curr list =
    match list with
    | [] -> acc :: []
    | x :: xs ->
      if x == curr then
        aux (x :: acc) curr xs
      else
        acc :: aux [x] x xs
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

(* With TCO *)
let encode (list : 'a list) : (int * 'a) list =
  let rec aux res acc list =
    match acc, list with
    | _, [] -> rev (acc :: res)
    | (n, x), y :: ys when x == y -> aux res (n + 1, x) ys
    | _, y :: ys -> aux (acc :: res) (1, y) ys
  in
  match list with [] -> [] | x :: xs -> aux [] (1, x) xs

let encode_notco (list : 'a list) : (int * 'a) list =
  let rec aux i curr list =
    match list with
    | [] -> (i, curr) :: []
    | x :: xs ->
      if x == curr then
        aux (i + 1) curr xs
      else
        (i, curr) :: aux 1 x xs
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

  |---- RESULT from previous exercise -------------------------------------------
  | # encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
  | - : (int * string) list =
  | [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
  |---- RESULT from previous exercise -------------------------------------------

    # encode2 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
    - : string rle list =
    [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
    Many (4, "e")]
*)

type 'a encode_t = EncOne of 'a | EncMany of (int * 'a)

let encode2 (list : 'a list) : 'a encode_t list =
  let result = encode list in
  let rec aux (acc : 'a encode_t list) (list : (int * 'a) list) : 'a encode_t list =
    match list with
    | [] -> rev acc
    | (1, x) :: xs -> aux (EncOne x :: acc) xs
    | (n, x) :: xs -> aux (EncMany (n, x) :: acc) xs
  in
  aux [] result

let encode2_notco (list : 'a list) : 'a encode_t list =
  let result = encode list in
  let rec aux list =
    match list with
    | [] -> []
    | (1, x) :: xs -> EncOne x :: aux xs
    | (n, x) :: xs -> EncMany (n, x) :: aux xs
  in
  aux result

(*
    12. Decode a run-length encoded list. (medium)

    Given a run-length code list generated as specified in the previous problem,
    construct its uncompressed version.

    # decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
    - : string list =
    ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
*)

(* With TCO *)
let decode (list : 'a encode_t list) : 'a list =
  let rec aux acc list =
    match list with
    | [] -> rev acc
    | EncOne x :: xs -> aux (x :: acc) xs
    | EncMany (1, x) :: xs -> aux (x :: acc) xs
    | EncMany (n, x) :: xs -> aux (x :: acc) (EncMany ((n - 1), x) :: xs)
  in
  aux [] list

let rec decode_notco (list: 'a encode_t list) : 'a list =
  let rec decode_elem elem =
    match elem with
    | EncOne x -> [x]
    | EncMany (0, _) -> []
    | EncMany (i, x) -> x :: decode_elem (EncMany ((i - 1), x))
  in
  match list with
  | [] -> []
  | x :: xs -> decode_elem x @ decode_notco xs

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

(* With TCO *)
let encode3 (list : 'a list) : 'a encode_t list =
  let rec aux acc i list =
    match i, list with
    | _, [] -> rev acc
    | 1, x :: [] -> rev @@ EncOne x :: acc
    | _, x :: [] -> rev @@ EncMany (i, x) :: acc
    | _, x :: y :: tail when x == y -> aux acc (i + 1) (y :: tail)
    | 1, x :: y :: tail -> aux (EncOne x :: acc) 1 (y :: tail)
    | _, x :: y :: tail -> aux (EncMany (i, x) :: acc) 1 (y :: tail)
  in
  aux [] 1 list

let encode3_notco (list : 'a list) : 'a encode_t list =
  let rec aux i curr list =
    match list with
    | [] ->
      let last_elem = if i == 1 then EncOne curr else EncMany (i, curr) in
      last_elem :: []
    | x :: xs ->
      if x == curr then
        aux (i + 1) curr xs
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

let duplicate (list : 'a list) : 'a list =
  let rec aux acc list =
    match list with
    | [] -> rev acc
    | x :: xs -> aux (x :: x :: acc) xs
  in
  aux [] list

let rec duplicate_notco (list : 'a list) : 'a list =
  match list with
  | [] -> []
  | x :: xs -> x :: x :: duplicate_notco xs

(*
    15. Replicate the elements of a list a given number of times. (medium)

    # replicate ["a"; "b"; "c"] 3;;
    - : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
*)

let replicate (n : int) (list : 'a list) : 'a list =
  let rec aux acc i n list =
    match i, list with
    | _, [] -> rev acc
    | 1, x :: xs -> aux (x :: acc) n n xs
    | _, x :: xs -> aux (x :: acc) (i - 1) n (x :: xs)
  in
  aux [] n n list

let rec replicate_notco (n : int) (list : 'a list) : 'a list =
  let rec replicate_elem i elem =
    match i with
    | 0 -> []
    | _ -> elem :: replicate_elem (i - 1) elem
  in
  match list with
  | [] -> []
  | x :: xs -> replicate_elem n x @ replicate_notco n xs

(*
    16. Drop every N'th element from a list. (medium)

    # drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
    - : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
*)

let drop (n : int) (list : 'a list) : 'a list =
  let rec aux acc i n list =
    match i, list with
    | _, [] -> rev acc
    | 1, _ :: xs -> aux acc n n xs
    | _, x :: xs -> aux (x :: acc) (i - 1) n xs
  in
  aux [] n n list

let drop_notco (n : int) (list : 'a list) : 'a list =
  let rec aux i n list =
    match (i, list) with
    | (_, []) -> []
    | (1, _ :: xs) -> aux n n xs
    | (_, x :: xs) -> x :: aux (i - 1) n xs
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
    match (i, list) with
    | (_, []) -> (rev acc, [])
    | (0, xs) -> (rev acc, xs)
    | (_, x :: xs) -> aux (x :: acc) (i - 1) xs
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

(* With TCO *)
let slice (first_pos : int) (last_pos : int) (list : 'a list) : 'a list =
  if length list == 0 then
     failwith "List is empty"
  else if last_pos > (length list) - 1 then
    failwith "Last position is out of bounds"
  else if first_pos < 0 then
    failwith "First position must be bigger than zero"
  else if last_pos < 0 then
    failwith "Last position must be bigger than zero"
  else if first_pos > last_pos then
    failwith "First position cannot be bigger than last position"
  else
    let rec aux acc first last list =
      match first, last, list with
      | _, _, [] -> failwith "List is empty"
      | 0, 0, x :: _ -> rev @@ x :: acc
      | 0, _, x :: xs -> aux (x :: acc) 0 (last - 1) xs
      | _, _, _ :: xs -> aux acc (first - 1) (last - 1) xs
    in
    aux [] first_pos last_pos list

let rec slice_notco (first : int) (last : int) (list : 'a list) : 'a list =
  match (first, last, list) with
  | (_, _, []) -> []
  | (0, 0, x :: _ ) -> x :: []
  | (0, _, x :: xs) -> x :: slice_notco 0 (last - 1) xs
  | (_, _, _ :: xs) -> slice_notco (first - 1) (last - 1) xs

let slice_old (first : int) (last : int) (list : 'a list) : 'a list =
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
  let rec aux acc i list =
    match (i, list) with
    | (_, []) -> acc
    | (0, xs) -> xs @ (rev acc)
    | (_, x :: xs) -> aux (x :: acc) (i - 1) xs
  in
  aux [] split_pos list

(*
    20. Remove the K'th element from a list. (easy)

    The first element of the list is numbered 0, the second 1,...

    # remove_at 1 ["a"; "b"; "c"; "d"];;
    - : string list = ["a"; "c"; "d"]
*)

let remove_at (pos : int) (list : 'a list) : 'a list =
  if length list == 0 then
    failwith "List is empty"
  else if pos < 0 then
    failwith "Position negative"
  else if pos > length list then
    failwith "Position out of bounds"
  else
    let rec aux acc n list =
        match n, list with
        | _, [] -> failwith "Position out of bounds"
        | 0, xs -> (rev acc) @ xs
        | _, x :: xs -> aux (x :: acc) (n - 1) xs
    in
    aux [] pos list
















let rec remove_at_notco (n : int) (list : 'a list) : 'a list =
  match (n, list) with
  | (_, []) -> []
  | (0, _ :: xs) -> xs
  | (_, x :: xs) -> x :: remove_at_notco (n - 1) xs

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

let rec insert_at (n : int) (elem : 'a) (list : 'a list) : 'a list =
  if n < 0 then
    failwith "Negative value provided as a list position"
  else
    match (n, list) with
    | (0, []) -> elem :: []
    | (_, []) -> failwith "Insert position out of bounds"
    | (0, xs) -> elem :: xs
    | (_, x :: xs) -> x :: insert_at (n - 1) elem xs

let insert_at_old (n : int) (elem: 'a) (list : 'a list) : 'a list =
  let rec aux i n elem list =
    match list with
    | [] -> []
    | x :: [] when i == (n - 1) -> x :: elem :: [] (* special case: append *)
    | x :: [] when i == n -> elem :: x :: [] (* special case: prepend *)
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
  if first == last then
    last :: []
  else if first > last then
    first :: range (first - 1) last
  else
    first :: range (first + 1) last

(*
    23. Extract a given number of randomly selected elements from a list. (medium)

    The selected items shall be returned in a list. We use the Random module but
    do not initialize it with Random.self_init for reproducibility.

    # rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
    - : string list = ["g"; "d"; "a"]
*)

(* Can't be recursive or the random seed is reseted *)
let rand_select (seed : int) (amt : int) (list : 'a list) : 'a list =
  Random.init seed;
  if amt < 0  || amt > (length list) then
    failwith "Invalid amount. Amount should be a integer from 0 to list length - 1"
  else if amt == 0 then
    []
  else
    let rec aux n list =
      if n == 0 then
        []
      else
        let rnd_pos = Random.int (length list) in
        let elem = at rnd_pos list in
        let rest = remove_at rnd_pos list in
        elem :: aux (n - 1) rest
    in
    aux amt list

(*
    24. Lotto: Draw N different random numbers from the set 1..M. (easy)

    The selected numbers shall be returned in a list.

    # lotto_select 6 49;;
    - : int list = [10; 20; 44; 22; 41; 2]
*)

(* Can't be recursive or the random seed is reseted *)
let lotto (seed : int) (amt : int) (limit : int) : int list =
  Random.init seed; (* Makes predicable random *)
  if amt < 0 then
    failwith "Amount is negative"
  else if amt > limit then
    failwith "Amount out of reach"
  else if amt == 0 then
    []
  else
    let rec aux acc n limit =
      if n == 0 then
        acc
      else
        let rnd_num = Random.int limit in
        let is_in_list = List.exists (fun x -> x == rnd_num) acc in
        if not is_in_list then
            aux (rnd_num :: acc) (n - 1) limit
        else
            aux acc n limit
    in
    rev @@ aux [] amt limit

let lotto_old (seed : int) (amt: int) (limit: int) : int list =
  Random.init seed;  (* Makes predicable random *)

  let rec get_unique_rnd acc limit =
    let rnd = Random.int limit in
    let is_in_list = List.exists (fun x -> x == rnd) acc in
    if not is_in_list then
      rnd
    else
      get_unique_rnd acc limit
  in

  let rec aux acc i size limit =
    if i < size then
      let unq = get_unique_rnd acc limit in
      aux (unq :: acc) (i + 1) size limit
    else
      acc
  in

  if amt > limit then
    failwith "Amount out of reach"
  else
    rev (aux [] 0 amt limit)

(*
    25. Generate a random permutation of the elements of a list. (easy)

    # permutation ["a"; "b"; "c"; "d"; "e"; "f"];;
    - : string list = ["a"; "e"; "f"; "b"; "d"; "c"]
*)

(* Can't be recursive or the random seed is reseted *)
let permutation (seed : int) (list : 'a list) : 'a list =
  Random.init seed;
  let rec aux list =
    if list == [] then
      []
    else
      let rnd_pos = Random.int @@ length list in
      let rnd_elem = at rnd_pos list in
      let rest = remove_at rnd_pos list in
      rnd_elem :: aux rest
  in
  aux list

(* Can't be recursive or the random seed is reseted *)
let permutation_old (seed : int) (list : 'a list) : 'a list =
  Random.init seed;

  let rec extract acc pos list =
    match (pos, list) with
    | (_, []) -> failwith "Out of bounds"
    | (0, x :: xs) -> (x, (rev acc) @ xs)
    | (_, x :: xs) -> extract (x :: acc) (pos - 1) xs
  in

  let get_rnd_elem_and_rest list =
    let rnd_pos = Random.int (length list) in
    extract [] rnd_pos list
  in

  let rec aux list =
    match list with
    | [] -> []
    | xs -> let (elem, rest) = get_rnd_elem_and_rest xs in elem :: aux rest
  in

  aux list

(*
    26. Generate the combinations of K distinct objects chosen from the N
    elements of a list. (medium)

    In how many ways can a committee of 3 be chosen from a group of 12 people? We
    all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the
    well-known binomial coefficients). For pure mathematicians, this result may
    be great. But we want to really generate all the possibilities in a list.

    # extract 2 ["a"; "b"; "c"; "d"];;
    - : string list list =
    [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
*)

(* let extract (pos : int) (list: 'a list) : 'a list list = [[]] *)

(*

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

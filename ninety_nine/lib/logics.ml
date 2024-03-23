(*
  Let us define a small "language" for boolean expressions containing variables:

  type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr

  A logical expression in two variables can then be written in prefix
  notation. For example, (a ∨ b) ∧ (a ∧ b) is written:

  # And (Or (Var "a", Var "b"), And (Var "a", Var "b"));;
  - : bool_expr = And (Or (Var "a", Var "b"), And (Var "a", Var "b"))

  46 & 47. Truth tables for logical expressions (2 variables). (medium)

  Define a function, table2 which returns the truth table of a given logical
  expression in two variables (specified as arguments). The return value must be
  a list of triples containing (value_of_a, value_of_b, value_of_expr).

  # table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")));;
  - : (bool * bool * bool) list =
  [(true, true, true); (true, false, true); (false, true, false);
  (false, false, false)]
*)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let eval_bool_expr2 (symbol1 : string) (value1: bool) (symbol2: string) (value2 : bool) (expr: bool_expr) : bool =
  let rec aux s1 v1 s2 v2 expr =
    match expr with
    | Var s -> if String.equal s s1 then v1 else v2
    | Not x -> not (aux s1 v1 s2 v2 x)
    | And (x, y) -> (aux s1 v1 s2 v2 x) && (aux s1 v1 s2 v2 y)
    | Or (x, y) -> (aux s1 v1 s2 v2 x) || (aux s1 v1 s2 v2 y)
  in
  aux symbol1 value1 symbol2 value2 expr

let truth_table2 (a : string) (b : string) (expr : bool_expr) : (bool * bool * bool) list =
  let res1 = eval_bool_expr2 a true b true expr in
  let res2 = eval_bool_expr2 a true b false expr in
  let res3 = eval_bool_expr2 a false b true expr in
  let res4 = eval_bool_expr2 a false b false expr in
  [(true, true, res1); (true, false, res2); (false, true, res3); (false, false, res4)]

(*
  48. Truth tables for logical expressions. (medium)

  Generalize the previous problem in such a way that the logical expression may
  contain any number of logical variables. Define table in a way that table
  variables expr returns the truth table for the expression expr, which contains
  the logical variables enumerated in variables.

  # table ["a"; "b"] (And (Var "a", Or (Var "a", Var "b")));;
  - : ((string * bool) list * bool) list =
  [([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
  ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)]

  # let a = Var "a" and b = Var "b" and c = Var "c" in
    table ["a"; "b"; "c"] (Or (And (a, Or (b,c)), Or (And (a,b), And (a,c))));;
  - : ((string * bool) list * bool) list =
  [ ([("a", true); ("b", true); ("c", true)], true)
  ; ([("a", true); ("b", true); ("c", false)], true)
  ; ([("a", true); ("b", false); ("c", true)], true)
  ; ([("a", true); ("b", false); ("c", false)], false)
  ; ([("a", false); ("b", true); ("c", true)], false)
  ; ([("a", false); ("b", true); ("c", false)], false)
  ; ([("a", false); ("b", false); ("c", true)], false)
  ; ([("a", false); ("b", false); ("c", false)], false)
  ]
*)

type truth_table_sample = (string * bool) list

let gen_table (symbols: string list) : truth_table_sample list =
  let rec gen tmp list =
    match list with
    | [] -> [List.rev tmp]
    | x :: xs -> gen ((x, true) :: tmp) xs @ gen ((x, false) :: tmp) xs
  in
  gen [] symbols

let eval_bool_expr (sample: truth_table_sample) (expr : bool_expr) : bool =
  let rec get_bool_for symbol sample =
    match sample with
    | [] -> failwith "Symbol not in the list"
    | (key, value) :: _ when String.equal key symbol -> value
    | _ :: xs -> get_bool_for symbol xs
  in
  let rec aux sample expr =
    match expr with
    | Var symbol -> get_bool_for symbol sample
    | Not x -> not (aux sample x)
    | And (x, y) -> (aux sample x) && (aux sample y)
    | Or (x, y) -> (aux sample x) || (aux sample y)
  in
  aux sample expr

let truth_table (symbols : string list) (expr : bool_expr) : (truth_table_sample * bool) list =
  let table = gen_table symbols in
  let rec aux acc table expr =
    match table with
    | [] -> List.rev acc
    | x :: xs -> let acc' = (x, eval_bool_expr x expr) :: acc in aux acc' xs expr
  in
  aux [] table expr

(*
49. Gray code. (medium)

An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,

n = 1: C(1) = ['0', '1'].
n = 2: C(2) = ['00', '01', '11', '10'].
n = 3: C(3) = ['000', '001', '011', '010', '110', '111', '101', '100'].

Find out the construction rules and write a function with the following specification: gray n returns the n-bit Gray code.

# gray 1;;
- : string list = ["0"; "1"]
# gray 2;;
- : string list = ["00"; "01"; "11"; "10"]
# gray 3;;
- : string list = ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"]
*)

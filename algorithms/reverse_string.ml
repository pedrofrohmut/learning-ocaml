(*
  reverse_string.ml
  Pedro Frohmut 2026 Copyrights

  Reverse a string provided
 *)


let inc (n: int): int = n + 1


(* Tail recursive - With auxiliar funciton and an accumulator *)
let reverse_string (str: string): string =
  let rec aux (acc: string) (i: int) (__str: string): string =
    let len = String.length str in
    if len = i then
      acc
    else
      let ch = String.get str i in
      let str_ch = String.make 1 ch in
      let new_acc = str_ch ^ acc in
      aux new_acc (inc i) str
  in
  aux "" 0 str


(* Not tail recursive - no auxiliar function needed *)
let rec reverse_string2 (str: string): string =
  let len = String.length str in
  if len = 0 then
    ""
  else
    let x = String.make 1 (String.get str 0) in
    let rest = String.sub str 1 (len - 1) in
    reverse_string2 rest ^ x


let test (reverse_func: string -> string) (str: string) (expected: string): unit =
  let reversed = reverse_func str in
  if reversed = expected then
    Printf.printf "[Success] The reversed of '%s' is '%s'\n" str reversed
  else
    Printf.printf "[Error] Expected reverse of '%s' to be '%s' but got '%s' instead\n" str expected reversed


let () =
  test reverse_string "hello" "olleh";
  test reverse_string "foobar" "raboof";
  test reverse_string "racecar" "racecar";
  test reverse_string "a" "a";
  test reverse_string "" "";
  test reverse_string "ab" "ba";
  test reverse_string "this is my super long string" "gnirts gnol repus ym si siht";
  test reverse_string "12345" "54321";

  test reverse_string2 "hello" "olleh";
  test reverse_string2 "foobar" "raboof";
  test reverse_string2 "racecar" "racecar";
  test reverse_string2 "a" "a";
  test reverse_string2 "" "";
  test reverse_string2 "ab" "ba";
  test reverse_string2 "this is my super long string" "gnirts gnol repus ym si siht";
  test reverse_string2 "12345" "54321"

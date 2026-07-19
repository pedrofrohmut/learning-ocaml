(*
  reverse_string.ml
  Pedro Frohmut 2026 Copyrights

  Reverse a string provided
 *)


let inc (n: int): int = n + 1


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


let test (str: string) (expected: string): unit =
  let reversed = reverse_string str in
  if reversed = expected then
    Printf.printf "[Success] The reversed of '%s' is '%s'\n" str reversed
  else
    Printf.printf "[Error] Expected reverse of '%s' to be '%s' but got '%s' instead\n" str expected reversed


let () =
  test "hello" "olleh";
  test "foobar" "raboof";
  test "racecar" "racecar";
  test "a" "a";
  test "" "";
  test "ab" "ba";
  test "this is my super long string" "gnirts gnol repus ym si siht";
  test "12345" "54321"

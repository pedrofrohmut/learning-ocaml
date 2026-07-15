let fibonacci (n : int) : int list =
  let rec aux (a : int) (b : int) (acc : int list) : int list =
    if b > n then
      List.rev (a :: acc)
    else
      let new_a = b in
      let new_b = (a + b) in
      let new_acc = a :: acc in
      aux new_a new_b new_acc
  in
  aux 1 1 []

let print_fibonacci (n : int) : string =
  let fibo_seq = fibonacci n in
  let str_seq = List.map string_of_int fibo_seq in
  let str_result = String.concat ", " str_seq in
  Printf.sprintf "Fibonacci of %d is [%s]" n str_result

let () =
  print_endline (print_fibonacci 100);

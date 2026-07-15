let rec factorial (n: int): int =
  if n == 1 then
    1
  else
    n * factorial (n - 1)

let () =
  Printf.printf "Factorial of %d is %d\n" 5 (factorial 5)

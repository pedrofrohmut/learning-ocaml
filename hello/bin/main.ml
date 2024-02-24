(* let new_line = Printf.printf "\n" *)

(* let hello name = Printf.printf "Hello, %s\n" name *)

(* let () = hello("Bob") *)

(* let () = Printf.printf "\n%s\n" Hello.En.hello *)

let () = Dream.(run (router [ get "/" (fun (_: request) -> html Hello.En.v) ]))

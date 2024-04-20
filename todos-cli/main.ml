let fname = "todos.txt"

let rec read_lines in_channel =
  try
    let line = input_line in_channel in
    print_endline line;
    read_lines in_channel
  with _ ->
    flush stdout;
    close_in in_channel

let main =
  if Array.length Sys.argv == 1 then
    failwith "No text provided for the todo"
  else if String.equal Sys.argv.(1) "" then
    failwith "Todo text cannot be blank"
  else
    let todo_text = Sys.argv.(1) in
    let out_channel = open_out_gen [Open_wronly; Open_append; Open_creat] 0o666 fname in
    output_string out_channel (todo_text ^ "\n");
    close_out out_channel;

    let in_channel = open_in fname in
    read_lines in_channel

let _ = main

(*
   TODOS:
     - Create the todos.txt if the file doesnt exist
     - Add an ID to each input (1 to n)
     - Read the last ID in the file and add new as (last_id + 1)
     - The file should contain ID,text as lines
     - Add options for add, list, delete and update
     - List output should be formatted
*)

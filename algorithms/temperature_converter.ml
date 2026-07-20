#!/usr/bin/env ocaml

(*
  temperature_converter.ml
  Pedro Frohmut 2026 Copyrights

  Command Line Interface program that show the temperatures in kelvin, celsius and
  farenheit based on the temperature used as argument.
 *)


module StringMap = Map.Make(String)

let print_single_arg (key: int) (value: string): unit =
  Printf.printf "Arg[%d] = %s\n" key Sys.argv.(key)

let print_args (args: string array): unit =
  print_endline "Printing args:";
  let len = Array.length args in
  for i = 1 to len - 1 do
    print_single_arg i args.(i)
  done

let print_help (): unit =
  print_endline {|Usage: temperature_converter [FLAGS] [temperature_value]

Get the temperatures values in kelvin, celcius and fahrenheit entering a single
value in any of the 3 types. Provide a flag to inform the type followed by the
number in this temperature.

List of options/flags:
  -h,--help 				Show this help.
  -v,--version 				Show version information.
  -k,--kelvin number		Provide a temperature in Kelvin.
  -c,--celsius number		Provide a temperatur in celcius.
  -f,--fahrenheit number	Provide a temperature in fahrenheit.
|}

let print_version_info (): unit =
  print_endline {|temperature_converter 1.0
Pedro Frohmut 2026 Copyrights.
License MIT.|}

let print_fail_number (): unit =
  print_endline "[Invalid] A number is required after a temperature flag to convert."

let print_temperatures (c: float) (k: float) (f: float): unit =
  Printf.printf "Celcius: %.2f, Kelvin: %.2f, Fahrenheit: %.2f\n" c k f

let kelvin_from_celcius (c: float): float =
  c +. 273.15

let fahrenheit_from_celcius (c: float): float =
  (c *. 1.8) +. 32.0

let celcius_from_fahrenheit (f: float): float =
  (f -. 32.0) *. (5.0 /. 9.0)

let kelvin_from_fahrenheit (f: float): float =
  ((f -. 32.0) *. (5.0 /. 9.0)) +. 273.15

let celcius_from_kelvin (k: float): float =
  k -. 273.15

let fahrenheit_from_kelvin (k: float): float =
  ((k -. 273.15) *. 1.8) +. 32.0

let print_convert_from (name: string) (n: float option): unit =
  match name, n with
  | _, None               -> print_fail_number ()
  | "celcius", Some x     -> print_temperatures x (kelvin_from_celcius x) (fahrenheit_from_celcius x)
  | "kelvin", Some x      -> print_temperatures (celcius_from_kelvin x) x (fahrenheit_from_kelvin x)
  | "fahrenheit", Some x  -> print_temperatures (celcius_from_fahrenheit x) (kelvin_from_fahrenheit x) x
  | _, Some x             -> print_endline "Invalid name for temperature."

let process_args (args: string array): unit =
  let len = Array.length args in
  if len = 1 then
    print_help ()
  else
    let first_arg = args.(1) in

    let opt_number = if len = 3 then Some (float_of_string args.(2)) else None in

    match first_arg with
    | "-h" | "--help"        -> print_help ()
    | "-v" | "--version"     -> print_version_info ()
    | "-c" | "--celcius"     -> print_convert_from "celcius" opt_number
    | "-f" | "--fahrenheit"  -> print_convert_from "fahrenheit" opt_number
    | "-k" | "--kelvin"      -> print_convert_from "kelvin" opt_number
    | _                      -> print_endline "Unknown option"

let main () =
  let args = Sys.argv in
  process_args args

let () = main ()

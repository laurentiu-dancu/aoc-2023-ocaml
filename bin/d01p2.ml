let is_digit c = 
  match c with
  | '0' .. '9' -> true
  | _ -> false

  let map_string_digit str =
    match str with
    | s when String.starts_with ~prefix:"one" s -> "1"
    | s when String.starts_with ~prefix:"two" s -> "2"
    | s when String.starts_with ~prefix:"three" s -> "3"
    | s when String.starts_with ~prefix:"four" s -> "4"
    | s when String.starts_with ~prefix:"five" s -> "5"
    | s when String.starts_with ~prefix:"six" s -> "6"
    | s when String.starts_with ~prefix:"seven" s -> "7"
    | s when String.starts_with ~prefix:"eight" s -> "8"
    | s when String.starts_with ~prefix:"nine" s -> "9"
    | _ -> ""

let get_digit_first str =
  match String.length str with
  | 0 -> ""
  | _ when is_digit str.[0] -> String.make 1 str.[0]
  | _ -> map_string_digit str

let rec extract_digits str =
  match str with
  | "" -> ""
  | str -> get_digit_first str ^ extract_digits (String.sub str 1 (String.length str - 1))

let extract_valid_calibration_number line =
  let str = extract_digits line in
  let str_len = String.length str in
  if str_len < 1 then
    0
  else
    int_of_string (String.make 1 str.[0] ^ String.make 1 str.[str_len - 1])

let rec sum_valid_calibration_numbers lines =
  match lines with
   | [] -> 0
   | hd :: tl -> extract_valid_calibration_number hd + sum_valid_calibration_numbers tl

open Aoc_utils

let () =
  let file = "res/d01p2/input.txt" in
  let lines = Io.read_lines file in
  let result = sum_valid_calibration_numbers lines in
  print_int result;
  print_newline();

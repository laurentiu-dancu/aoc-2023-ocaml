let is_digit c = 
  match c with
  | '0' .. '9' -> true
  | _ -> false

 let concat_if_digit acc c =
  if is_digit c then
    acc ^ String.make 1 c
  else
    acc

let extract_digits str =
  String.fold_left concat_if_digit "" str

let extract_valid_calibration_number line =
  let str = extract_digits line in
  let str_len = String.length str in
  if str_len < 1 then
    0
  else
    int_of_string (
      String.make 1 (String.get str 0) ^ 
      String.make 1 (String.get str (str_len - 1))
    )

let rec sum_valid_calibration_numbers lines =
  match lines with
   | [] -> 0
   | hd :: tl -> extract_valid_calibration_number hd + sum_valid_calibration_numbers tl

open Aoc_utils

let () =
  let file = "res/d01/input.txt" in
  let lines = Io.read_lines file in
  let result = sum_valid_calibration_numbers lines in
  print_int result;
  print_newline();

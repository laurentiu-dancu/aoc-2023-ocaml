open Aoc_utils

let is_star c =
  match c with
  | '*' -> true
  | _ -> false

let rec parse_number_left line cursor =
  if cursor < 0 then
    ""
  else
    let c = line.[cursor] in
    if Chars.is_digit c then
      parse_number_left line (cursor - 1) ^ String.make 1 c
    else
      ""

let rec parse_number_right line cursor cursor_max =
  if cursor >= cursor_max then
    ""
 else
  let c = line.[cursor] in
  if Chars.is_digit c then
    String.make 1 c ^ parse_number_right line (cursor + 1) cursor_max
  else
    ""

let string_to_int str =
  match str with
  | "" -> 0
  | str -> int_of_string str

let is_digit_line line cursor = Chars.is_digit line.[cursor]

let rec count_values list =
  match list with
  | [] -> 0
  | hd :: tl -> (if hd != 0 then 1 else 0) + count_values tl

let rec multiply_nonzero list =
  match list with
  | [] -> 1
  | hd :: tl -> (if hd != 0 then hd else 1) * multiply_nonzero tl

let parse_symbol top mid bot cursor max_cursor =
  let (top_left, top_right) = if is_digit_line top cursor then
    (string_to_int (parse_number_left top (cursor - 1) ^ parse_number_right top cursor max_cursor), 0)
  else
    (string_to_int (parse_number_left top (cursor - 1)), string_to_int (parse_number_right top (cursor + 1) max_cursor))
  in
  let (bot_left, bot_right) = if is_digit_line bot cursor then
    (string_to_int (parse_number_left bot (cursor - 1) ^ parse_number_right bot cursor max_cursor), 0)
  else
    (string_to_int (parse_number_left bot (cursor - 1)), string_to_int (parse_number_right bot (cursor + 1) max_cursor))
  in
  let left_part = string_to_int (parse_number_left mid (cursor - 1)) in
  let right_part = string_to_int (parse_number_right mid (cursor + 1) max_cursor) in
  let int_list = top_left :: top_right :: bot_left :: bot_right :: left_part :: right_part :: [] in
  
  if (count_values int_list == 2) then multiply_nonzero int_list else 0

let sum_when_char top mid bot cursor max_cursor =
  let c = mid.[cursor] in
  match c with
  | s when is_star s -> parse_symbol top mid bot cursor max_cursor
  | _ -> 0

let rec parse_set_inner top mid bot cursor end_cursor =
  match cursor with
  | _ when cursor >= end_cursor -> 0
  | _ -> sum_when_char top mid bot cursor end_cursor + parse_set_inner top mid bot (cursor + 1) end_cursor

let parse_set top mid bot =
  let end_cursor = (String.length mid) in
  parse_set_inner top mid bot 1 end_cursor

let rec parse_lines lines =
  match lines with
  | [] -> 0
  | _ :: [] -> 0
  | _ :: _ :: [] -> 0
  | top :: mid :: bot :: rest -> (parse_set top mid bot) + (parse_lines (mid :: bot :: rest))

let () =
  let file = "res/d03.txt" in
  let lines = Io.read_lines file in
  let result = parse_lines lines in
  print_int result;
  print_newline();

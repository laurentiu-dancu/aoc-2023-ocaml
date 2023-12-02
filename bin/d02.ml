open Aoc_utils

let trim_string str = 
  match str with
  | "" -> ""
  | str -> String.sub str 1 ((String.length str) - 1)

let is_valid_cube cube =
  let amount_string = (List.hd cube) in
  let amount = int_of_string amount_string in
  let color = List.hd (List.tl cube) in

  match color with
  | "blue" -> amount <= 14
  | "green" -> amount <= 13
  | "red" -> amount <= 12
  | _ -> true

let rec is_valid_game cubes =
  match cubes with
  | [] -> true
  | hd :: tl -> 
      let str = trim_string hd in
      if is_valid_cube (String.split_on_char ' ' str) 
      then is_valid_game tl 
      else false

let rec is_all_valid cubes_sets = 
  match cubes_sets with
  | [] -> true
  | hd :: tl -> 
      if is_valid_game (String.split_on_char ',' hd)
      then is_all_valid tl 
    else false

let valid_game_id line =
  let parts = String.split_on_char ':' line in
  let game = List.hd parts in
  let all_cubes = List.hd (List.tl parts) in
  match is_all_valid (String.split_on_char ';' all_cubes) with
  | false -> 0
  | true -> int_of_string (List.hd (List.tl (String.split_on_char ' ' game)))

let rec sum_of_valid_games lines =
  match lines with
  | [] -> 0
  | hd :: tl -> valid_game_id hd + sum_of_valid_games tl

let () =
  let file = "res/d02/input.txt" in
  let lines = Io.read_lines file in
  let result = sum_of_valid_games lines in
  print_int result;
  print_newline();

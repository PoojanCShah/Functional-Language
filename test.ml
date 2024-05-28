open Parser
open Lexer

let parse code = 
  let lexbuf = Lexing.from_string code in
  Parser.root Lexer.token lexbuf;;

let rec exp_to_string e indent =
  let ind = String.make indent ' ' in
  match e with
  | V s -> ind ^ "V(\"" ^ s ^ "\")"
  | Num n -> ind ^ "Num(" ^ string_of_int n ^ ")"
  | Bl b -> ind ^ "Bl(" ^ string_of_bool b ^ ")"
  | Plus (e1, e2) -> ind ^ "Plus(\n" ^ exp_to_string e1 (indent + 2) ^ ",\n" ^ exp_to_string e2 (indent + 2) ^ "\n" ^ ind ^ ")"
  | Minus (e1, e2) -> ind ^ "Minus(\n" ^ exp_to_string e1 (indent + 2) ^ ",\n" ^ exp_to_string e2 (indent + 2) ^ "\n" ^ ind ^ ")"
  | Times (e1, e2) -> ind ^ "Times(\n" ^ exp_to_string e1 (indent + 2) ^ ",\n" ^ exp_to_string e2 (indent + 2) ^ "\n" ^ ind ^ ")"
  | And (e1, e2) -> ind ^ "And(\n" ^ exp_to_string e1 (indent + 2) ^ ",\n" ^ exp_to_string e2 (indent + 2) ^ "\n" ^ ind ^ ")"
  | Or (e1, e2) -> ind ^ "Or(\n" ^ exp_to_string e1 (indent + 2) ^ ",\n" ^ exp_to_string e2 (indent + 2) ^ "\n" ^ ind ^ ")"
  | Not e -> ind ^ "Not(\n" ^ exp_to_string e (indent + 2) ^ "\n" ^ ind ^ ")"
  | Eq (e1, e2) -> ind ^ "Eq(\n" ^ exp_to_string e1 (indent + 2) ^ ",\n" ^ exp_to_string e2 (indent + 2) ^ "\n" ^ ind ^ ")"
  | Gt (e1, e2) -> ind ^ "Gt(\n" ^ exp_to_string e1 (indent + 2) ^ ",\n" ^ exp_to_string e2 (indent + 2) ^ "\n" ^ ind ^ ")"
  | Abs (s, e) -> ind ^ "Abs(\"" ^ s ^ "\",\n" ^ exp_to_string e (indent + 2) ^ "\n" ^ ind ^ ")"
  | App (e1, e2) -> ind ^ "App(\n" ^ exp_to_string e1 (indent + 2) ^ ",\n" ^ exp_to_string e2 (indent + 2) ^ "\n" ^ ind ^ ")"
  | IfTE (e1, e2, e3) -> ind ^ "IfTE(\n" ^ exp_to_string e1 (indent + 2) ^ ",\n" ^ exp_to_string e2 (indent + 2) ^ ",\n" ^ exp_to_string e3 (indent + 2) ^ "\n" ^ ind ^ ")"
  | Pair (e1, e2) -> ind ^ "Pair(\n" ^ exp_to_string e1 (indent + 2) ^ ",\n" ^ exp_to_string e2 (indent + 2) ^ "\n" ^ ind ^ ")"
  | Fst e -> ind ^ "Fst(\n" ^ exp_to_string e (indent + 2) ^ "\n" ^ ind ^ ")"
  | Snd e -> ind ^ "Snd(\n" ^ exp_to_string e (indent + 2) ^ "\n" ^ ind ^ ")"


let rec repl () =
  let ic = open_in Sys.argv.(1) in
  let rec read_all acc =
    try
      let line = input_line ic in
      read_all (acc ^ line ^ "\n")
    with End_of_file ->
      acc
  in
  let all_lines = read_all "" in
  let lexbuf = Lexing.from_string all_lines in
  let parsed = Parser.root Lexer.token lexbuf in
  let tree = exp_to_string parsed 0 in
  print_endline tree;
  print_newline ();
  close_in ic

let _ = repl ()


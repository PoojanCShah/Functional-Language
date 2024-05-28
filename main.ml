open List
open Parser
open Lexer

exception STUCK


type opcode = 
  | LOOKUP of string
  | LDN of int
  | LDB of bool
  | PLUS
  | TIMES
  | MINUS
  | AND
  | OR
  | NOT
  | EQ
  | GT
  | APP
  | MKCLOS of string * (opcode list)
  | RET
  | COND of (opcode list) * (opcode list)
  | MKPAIR 
  | FST
  | SND 

type code = 
  | C of opcode list


let rec compile e = match e with
  | V x -> [LOOKUP(x)]
  | Num n -> [LDN n]
  | Bl b -> [LDB b]
  | Plus(e1,e2) -> (compile e1) @ (compile e2) @ [PLUS]
  | Minus(e1,e2) -> (compile e1) @ (compile e2) @ [MINUS]
  | Times(e1,e2) -> (compile e1) @ (compile e2) @ [TIMES]
  | And(e1,e2) -> (compile e1) @ (compile e2) @ [AND]
  | Or(e1,e2) -> (compile e1) @ (compile e2) @ [OR]
  | Not(e') -> (compile e') @ [NOT]   
  | Eq(e1,e2) -> (compile e1) @ (compile e2) @ [EQ]
  | Gt(e1,e2) -> (compile e1) @ (compile e2) @ [GT]
  | Abs(x,e') -> [MKCLOS(x, (compile e') @ [RET])]
  | App(e1,e2) -> (compile e1) @ ( compile e2 ) @ [APP]
  | IfTE(e0,e1,e2) -> (compile e0) @ [COND(compile e1, compile e2)]
  | Pair(e1,e2) -> (compile e1) @ (compile e2) @ [MKPAIR]
  | Fst(e') -> (compile e') @ [FST]
  | Snd(e') -> (compile e') @ [SND]  

type ans = 
  | N of int
  | B of bool
  | P of ans*ans 
  | Clos of string * code * ((string*ans) list)


(* let rec print_ans a = 
  match a with
  | N n -> print_int n
  | B b -> print_string (string_of_bool b)
  | P (a1,a2) -> print_string "("; print_ans a1; print_string ","; print_ans a2; print_string ")"
  | Clos (x,c,g) ->  *)


type table = 
  | Table of (string * ans) list


let rec lookup x l = 
  match l with Table g ->
  match g with
  | [] -> raise STUCK
  | (h,a) :: t -> if (x = h) then a else lookup x (Table t)


type stack = 
  | S of ans list


type env = 
  | E of (string * ans)  list


type dump = 
  | D of (stack * env * code) list 


type secd = 
  | SECD of stack * env * code * dump


type vclosi = VCLosI of string * code * table


let rec execute machine = 
  match machine with SECD(S s, E g, C c, D d) ->
  match s,g,c,d with

  | h :: t,g,[],d-> 
    h
  | s,g,LOOKUP(x) :: c',d -> 
    let a = lookup x (Table g) in
    execute (SECD(S (a :: s) , E g, C c' , D d))

  | s, g, (LDN n) :: c' , d ->
    execute (SECD ( S ((N n)::s) , E g, C c', D d))
  | s, g, (LDB b) :: c' , d ->
    execute (SECD ( S ((B b)::s) , E g, C c', D d))  

  | (N n2) :: (N n1) ::  s' , g, PLUS :: c', d ->
    execute (SECD( S ( N (n1+n2) :: s') , E g, C c' , D d))

  | (N n2) :: (N n1) ::  s' , g, TIMES :: c', d ->
    execute (SECD( S ( N (n1*n2) :: s') , E g, C c' , D d))

  | (N n2) :: (N n1) ::  s' , g, MINUS :: c', d ->
    execute (SECD( S ( N (n1- n2) :: s') , E g, C c' , D d))

  | (B b2) :: (B b1) ::  s' , g, AND :: c', d ->
    execute (SECD( S ( B (b1 && b2) :: s') , E g, C c' , D d))

  | (B b2) :: (B b1) ::  s' , g, OR :: c', d ->
    execute (SECD( S ( B (b1 || b2) :: s') , E g, C c' , D d))

  | (B b) :: s' , g, NOT :: c', d ->
    execute (SECD( S ( B (not b)  :: s') , E g, C c' , D d))


  | (N n2) :: (N n1) ::  s' , g, EQ :: c', d ->
    execute (SECD( S ( B (n1 = n2) :: s') , E g, C c' , D d))

  | (B n2) :: (B n1) ::  s' , g, EQ :: c', d ->
    execute (SECD( S ( B (n1 = n2) :: s') , E g, C c' , D d))

  | (N n2) :: (N n1) ::  s' , g, GT :: c', d ->
    execute (SECD( S ( B (n1 > n2) :: s') , E g, C c' , D d))

  | s,g,MKCLOS(x, c') :: c'', d ->
    execute (SECD( S( Clos (x, C c', g) :: s) ,E g, C c'', D d))
  | a :: Clos(x, C c', g') :: s' , g, APP :: c'',  d ->
    execute (SECD(S [], E ((x,a) :: g') , C c' , D ((S s', E g, C c'') :: d)))
  | a :: s' , g'', RET :: c', (S s, E g, C c'') :: d' ->
    execute (SECD(S (a :: s), E g, C c'', D d')) 


  | (B true) :: s' , g, COND(c1,c2) :: c' , d ->
    execute  (SECD (S s' , E g, C (c1 @ c'), D d))

  | (B false) :: s' , g, COND(c1,c2) :: c' , d ->
    execute  (SECD (S s' , E g, C (c2 @ c'), D d))

  | (P (a1 , a2)) :: s' , g,  FST :: c' , d ->
    execute (SECD (S (a1 ::s'), E g, C c', D d ))

  | (P (a1 , a2)) :: s' , g,  SND :: c' , d ->
    execute (SECD (S (a2 ::s'), E g, C c', D d ))

  | (a2 :: a1 :: s') , g, MKPAIR :: c' , d ->
    execute (SECD(S (P(a1,a2) :: s') , E g, C c', D d))


  | _ -> raise STUCK


let eval e = 
  let c = compile e in
  let machine = SECD(S [], E [], C c , D []) in
  execute machine 


let rec print_tree indent exp =
  let print_indent () = print_string (String.make indent ' ') in
  match exp with
  | V s -> print_indent (); print_endline ("V " ^ s)
  | Num n -> print_indent (); print_endline ("Num " ^ string_of_int n)
  | Bl b -> print_indent (); print_endline ("Bl " ^ string_of_bool b)
  | Plus (e1, e2) -> print_indent (); print_endline "Plus"; print_tree (indent + 2) e1; print_tree (indent + 2) e2
  | Minus (e1, e2) -> print_indent (); print_endline "Minus"; print_tree (indent + 2) e1; print_tree (indent + 2) e2
  | Times (e1, e2) -> print_indent (); print_endline "Times"; print_tree (indent + 2) e1; print_tree (indent + 2) e2
  | And (e1, e2) -> print_indent (); print_endline "And"; print_tree (indent + 2) e1; print_tree (indent + 2) e2
  | Or (e1, e2) -> print_indent (); print_endline "Or"; print_tree (indent + 2) e1; print_tree (indent + 2) e2
  | Not e -> print_indent (); print_endline "Not"; print_tree (indent + 2) e
  | Eq (e1, e2) -> print_indent (); print_endline "Eq"; print_tree (indent + 2) e1; print_tree (indent + 2) e2
  | Gt (e1, e2) -> print_indent (); print_endline "Gt"; print_tree (indent + 2) e1; print_tree (indent + 2) e2
  | Abs (s, e) -> print_indent (); print_endline ("Abs " ^ s); print_tree (indent + 2) e
  | App (e1, e2) -> print_indent (); print_endline "App"; print_tree (indent + 2) e1; print_tree (indent + 2) e2
  | IfTE (e1, e2, e3) -> print_indent (); print_endline "IfTE"; print_tree (indent + 2) e1; print_tree (indent + 2) e2; print_tree (indent + 2) e3
  | Pair (e1, e2) -> print_indent (); print_endline "Pair"; print_tree (indent + 2) e1; print_tree (indent + 2) e2
  | Fst e -> print_indent (); print_endline "Fst"; print_tree (indent + 2) e
  | Snd e -> print_indent (); print_endline "Snd"; print_tree (indent + 2) e


let rec print_opcode_list op_list =
  match op_list with
  | [] -> ()
  | h::t -> 
    (match h with
     | LOOKUP s -> print_string ("LOOKUP " ^ s ^ ",")
     | LDN i -> print_string ("LDN " ^ string_of_int i ^ ",")
     | LDB b -> print_string ("LDB " ^ string_of_bool b ^ ",")
     | PLUS -> print_string ("PLUS"^ ",")
     | TIMES -> print_string ("TIMES"^ ",")
     | MINUS -> print_string ("MINUS" ^ ",")
     | AND -> print_string ("AND"^ ",")
     | OR -> print_string ("OR"^ ",")
     | NOT -> print_string ("NOT"^ ",")
     | EQ -> print_string ("EQ"^ ",")
     | GT -> print_string ("GT" ^ ",")
     | APP -> print_string ("APP"^ ",")
     | MKCLOS (s, op) -> print_string ("MKCLOS " ^ s ^ ","); print_string "["; print_opcode_list op; print_string "]";
     | RET -> print_string "RET"
     | COND (op1, op2) -> print_string ("COND"^ ","); print_string "["; print_opcode_list op1; print_string "]["; print_opcode_list op2 ; print_string "]";
     | MKPAIR -> print_string  ("MKPAIR"^ ",")
     | FST -> print_string ("FST"^ ",")
     | SND -> print_string ("SND"^ ","));
    print_opcode_list t

let print_code code =
  match code with
  | C op_list -> print_opcode_list op_list

  let rec print_ans a = 
    match a with
    | N n -> print_int n
    | B b -> print_string (string_of_bool b)
    | P (a1,a2) -> print_string "("; print_ans a1; print_string ","; print_ans a2; print_string ")"
    | Clos (x,c,g) -> print_string ("Clos(" ^ x ^ ", ["); print_code c; print_string "], ["; print_table g; print_string "])"
  
  
  and print_table g = 
    match g with
    | [] -> ()
    | (h,a)::t -> print_string ("(" ^ h ^ ", "); print_ans a; print_string "); "; print_table t

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
  let result = eval parsed in
  (* print_tree  0 parsed;
  print_opcode_list (compile parsed); *)
  print_ans result;
  print_newline ();
  close_in ic



let _ = repl ()


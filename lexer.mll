{
open Parser
}

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf } (* Skip whitespace *)
  | "true" { BOOL(true) }
  | "false" { BOOL(false) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | ',' { COMMA }
    | (['0'-'9']+ )as lxm { NUM(int_of_string lxm) }
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }
  | '=' { EQ }
  | '>' { GT }
  | '\\' { ABS }
  | ',' { APP }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "first" { FST }
  | "second" { SND }
  | "fun" { FUN }
  | "let" { LET }
  | "in" { IN }
  | "rec" { REC }
  | "->" { ARROW }
    | ['a'-'z' 'A'-'Z']+ as lxm { VAR(lxm) }
  | eof { EOF }
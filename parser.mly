%{
    

    type exp = 
  | V of string
  | Num of int
  | Bl of bool
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | And of exp * exp
  | Or of exp * exp
  | Not of exp
  | Eq of exp * exp
  | Gt of exp * exp 
  | Abs of string*exp
  | App of exp*exp 
  | IfTE of exp*exp*exp
  | Pair of exp*exp
  | Fst of exp
  | Snd of exp 


(* let y = Abs("t", App(Abs("f", App(V "t", App(V "f", V "f"))), Abs("f", App(V "t", App(V "f", V "f"))))) *)




%}

%token <string>VAR
%token <int>NUM
%token <bool> BOOL
%token PLUS MINUS TIMES AND OR NOT EQ GT ABS APP EOF ARROW LPAREN RPAREN IF THEN ELSE PAIR FST SND FUN LET IN COMMA REC




%start root
%type <exp> root

%%

root:
  | exp EOF { $1 }

exp : 
  | let_exp { $1 }

let_exp:
  | app_exp { $1 }
  | LET VAR EQ app_exp IN let_exp { App(Abs($2,$6),$4) }
                                          


app_exp:
  | abs_exp { $1 }
  | app_exp abs_exp { App($1,$2) }

abs_exp:
  | tup_exp { $1 }
  | FUN VAR ARROW abs_exp { Abs($2,$4) }
  | LET REC VAR EQ FUN VAR ARROW abs_exp IN  tup_exp { 

App (  Abs("t", App ( Abs($3 , $10)  , App(V"t",V"t")  ))     ,         Abs("f" , Abs($6 , App ( Abs($3, $8) , App (V"f", V"f"))))              )

      }

tup_exp:
  | ifte_exp { $1 }
  | LPAREN ifte_exp COMMA tup_exp RPAREN { Pair($2,$4) }

ifte_exp:
  | comp_exp { $1 }
  | IF comp_exp THEN ifte_exp ELSE ifte_exp { IfTE($2,$4,$6) }

comp_exp:
  | arith_exp { $1 }
  | arith_exp EQ comp_exp { Eq($1,$3) }
  | arith_exp GT comp_exp { Gt($1,$3) }

arith_exp:
  | term { $1 }
  | arith_exp PLUS term { Plus($1,$3) }
  | arith_exp MINUS term { Minus($1,$3) }
  | arith_exp OR term { Or($1,$3) }

term:
  | factor { $1 }
  | term TIMES factor { Times($1,$3) }
  | term AND factor { And($1,$3) }

factor:
  | nfactor { $1 }
  | NOT nfactor { Not($2) }
  | FST nfactor { Fst($2) }
  | SND nfactor { Snd($2) }

nfactor:
  | VAR { V($1) }
  | NUM { Num($1) }
  | BOOL { Bl($1) }
  | LPAREN exp RPAREN { $2 }




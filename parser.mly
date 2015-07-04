%{
  open Syntax
  exception ParserError of string
%}

%token <string> IDENT
%token <char> ALPHA
%token ARROW EOF
%token S0
%token FINAL

%type <Syntax.expr list> main
%start main

%%

main:
| expr_list EOF
    { $1 }

expr_list:
|
    { [] }
| arrow expr_list
    { $1 :: $2 }
| S0 s0_list expr_list
    { $2 @ $3 }
| FINAL f_list expr_list
    { $2 @ $3 }

s0_list:
|
    { [] }
| IDENT s0_list
    { (S0 $1) :: $2 }

f_list:
|
    { [] }
| IDENT f_list
    { (F $1) :: $2 }
| error
    { raise (ParserError "hoge") }

arrow:
| IDENT ALPHA ARROW IDENT
    { D (($1, $2), $4) }

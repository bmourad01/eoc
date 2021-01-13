%{
    open R
%}

%token EOF
%token <int> INT
%token <string> VAR
%token PLUS MINUS
%token LPAREN RPAREN
%token READ

%start prog
%type <R.t> prog

%%

prog:
  | exp EOF
    { Program ((), $1) }

exp:
  | INT
    { Int $1 }
  | prim
    { Prim $1 }

prim:
  | LPAREN READ RPAREN
    { Read }
  | LPAREN MINUS exp RPAREN
    { Minus $3 }
  | LPAREN PLUS exp exp RPAREN
    { Plus ($3, $4) }

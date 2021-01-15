%{
    open R
%}

%token EOF
%token <int> INT
%token <string> VAR
%token PLUS MINUS STAR
%token LPAREN RPAREN LSQUARE RSQUARE
%token READ LET

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
  | VAR
    { Var $1 }
  | LPAREN LET LPAREN LSQUARE v = VAR e1 = exp RSQUARE RPAREN e2 = exp RPAREN
    { Let (v, e1, e2) }

prim:
  | LPAREN READ RPAREN
    { Read }
  | LPAREN MINUS exp RPAREN
    { Minus $3 }
  | LPAREN PLUS exp exp RPAREN
    { Plus ($3, $4) }
  | LPAREN MINUS exp exp RPAREN
    { Subtract ($3, $4) }
  | LPAREN STAR exp exp RPAREN
    { Mult ($3, $4) }

%{
    open R
%}

%token EOF
%token <int> INT
%token <string> VAR
%token PLUS MINUS STAR
%token LPAREN RPAREN LSQUARE RSQUARE
%token READ LET
%token TRUE FALSE
%token EQ LT LE GT GE NOT AND OR IF

%start prog
%type <R.t> prog

%%

prog:
  | exp EOF
    {
      let open Core_kernel in
      Program ({typ= type_check_rvar_exp String.Map.empty $1}, $1)
    }

exp:
  | INT
    { Int $1 }
  | TRUE
    { Bool true }
  | FALSE
    { Bool false }
  | prim
    { Prim $1 }
  | VAR
    { Var $1 }
  | LPAREN LET LPAREN LSQUARE v = VAR e1 = exp RSQUARE RPAREN e2 = exp RPAREN
    { Let (v, e1, e2) }
  | LPAREN IF exp exp exp RPAREN
    { If ($3, $4, $5) }

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
  | LPAREN EQ exp exp RPAREN
    { Eq ($3, $4) }
  | LPAREN LT exp exp RPAREN
    { Lt ($3, $4) }
  | LPAREN LE exp exp RPAREN
    { Le ($3, $4) }
  | LPAREN GT exp exp RPAREN
    { Gt ($3, $4) }
  | LPAREN GE exp exp RPAREN
    { Ge ($3, $4) }
  | LPAREN NOT exp RPAREN
    { Not $3 }
  | LPAREN AND exp exp RPAREN
    { And ($3, $4) }
  | LPAREN OR exp exp RPAREN
    { Or ($3, $4) }

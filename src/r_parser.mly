%{
    open R

    let lassoc e1 e2 es ~f =
      let open Core_kernel in
      match List.fold (e2 :: es) ~init:e1 ~f:(fun acc e -> Prim (f acc e)) with
      | Prim p -> p
      | _ -> assert false

    let rassoc e1 e2 es ~f =
      let open Core_kernel in
      let (init, l) =
        let l = List.rev es in
        (List.hd_exn l, List.drop l 1 |> List.rev)
      in
      f e1 (List.fold_right (e2 :: l) ~init ~f:(fun e acc -> Prim (f e acc)))
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
      Program ({typ= type_check_exp empty_var_env $1}, $1)
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
  | LPAREN PLUS exp exp nonempty_list(exp) RPAREN
    { lassoc $3 $4 $5 ~f:(fun e1 e2 -> Plus (e1, e2)) }
  | LPAREN MINUS exp exp RPAREN
    { Subtract ($3, $4) }
  | LPAREN MINUS exp exp nonempty_list(exp) RPAREN
    { lassoc $3 $4 $5 ~f:(fun e1 e2 -> Subtract (e1, e2)) }
  | LPAREN STAR exp exp RPAREN
    { Mult ($3, $4) }
  | LPAREN STAR exp exp nonempty_list(exp) RPAREN
    { lassoc $3 $4 $5 ~f:(fun e1 e2 -> Mult (e1, e2)) }
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
  | LPAREN AND exp exp nonempty_list(exp) RPAREN
    { rassoc $3 $4 $5 ~f:(fun e1 e2 -> And (e1, e2)) }
  | LPAREN OR exp exp RPAREN
    { Or ($3, $4) }
  | LPAREN OR exp exp nonempty_list(exp) RPAREN
    { rassoc $3 $4 $5 ~f:(fun e1 e2 -> Or (e1, e2)) }

%{
    open R

    let lassoc_prim e1 e2 es ~f =
      let open Core_kernel in
      match List.fold (e2 :: es) ~init:e1 ~f:(fun acc e -> Prim (f acc e)) with
      | Prim p -> p
      | _ -> assert false

    let rassoc_prim e1 e2 es ~f =
      let open Core_kernel in
      let (init, l) =
        let l = List.rev es in
        (List.hd_exn l, List.drop l 1 |> List.rev)
      in
      f e1 (List.fold_right (e2 :: l) ~init ~f:(fun e acc -> Prim (f e acc)))
%}

%token EOF
%token DEFINE LAMBDA
%token LPAREN RPAREN LSQUARE RSQUARE
%token <int64> INT
%token <string> VAR
%token PLUS MINUS STAR FSLASH REM LAND LOR LXOR LNOT 
%token SETBANG BEGIN WHEN UNLESS PRINT WHILE
%token VECTOR VECTORLENGTH VECTORREF VECTORSETBANG PROCEDUREARITY VOID
%token TINTEGER TBOOLEAN TVOID TVECTOR ARROW COLON
%token READ LET
%token TRUE FALSE
%token EQ LT LE GT GE NOT AND OR IF

%start prog
%type <R_typed.t> prog

%%

prog:
  | nonempty_list(def) exp EOF
    {
      let open Core_kernel in
      R_typed.type_check (Program ((), $1, $2))
    }
  | exp EOF
    {
      let open Core_kernel in
      R_typed.type_check (Program ((), [], $1))
    }

let_arg:
  | LSQUARE v = VAR e = exp RSQUARE
    { (v, e) }

typ:
  | TINTEGER
    { Type.Integer }
  | TBOOLEAN
    { Type.Boolean }
  | TVOID
    { Type.Void }
  | LPAREN TVECTOR list(typ) RPAREN
    { Type.Vector $3 }
  | LPAREN ARROW typ RPAREN
    { Type.Arrow ([], $3) }
  | LPAREN typ ARROW separated_nonempty_list(ARROW, typ) RPAREN
    {
      let open Core_kernel in
      let ts = List.drop_last_exn $4 in
      Type.Arrow ($2 :: ts, List.last_exn $4)
    }

def_arg:
  | LSQUARE VAR COLON typ RSQUARE
    { ($2, $4) }

def:
  | DEFINE LPAREN v = VAR args = list(def_arg) RPAREN COLON t = typ e = exp RPAREN
    {
      let open Core_kernel in
      let s = Hash_set.create (module String) in
      List.iter args ~f:(fun (x, _) ->
          match Hash_set.strict_add s x with
          | Error _ ->
             invalid_arg
               ("define: function " ^ v ^ ", arg " ^ x ^ " cannot be redefined")
          | Ok () -> ());
      Def (v, args, t, e)
    }

atom_exp:
  | INT
    { Int $1 }
  | TRUE
    { Bool true }
  | FALSE
    { Bool false }
  | LPAREN VOID RPAREN
    { Void }
  | VAR
    { Var $1 }

let_exp:
  | LPAREN LET LPAREN args = nonempty_list(let_arg) RPAREN body = exp RPAREN
    {
      let open Core_kernel in
      let s = Hash_set.create (module String) in
      List.iter args ~f:(fun (v, e) ->
          match Hash_set.strict_add s v with
          | Error _ -> invalid_arg ("let: var " ^ v ^ " cannot be shadowed")
          | Ok () -> ());
      List.fold_right args ~init:body ~f:(fun (v, e) acc -> Let (v, e, acc))
    }
  | LPAREN LET STAR LPAREN args = nonempty_list(let_arg) RPAREN body = exp RPAREN
    {
      let open Core_kernel in
      List.fold_right args ~init:body ~f:(fun (v, e) acc -> Let (v, e, acc))
    }

exp:
  | atom_exp
    { $1 }
  | prim
    { Prim $1 }
  | let_exp
    { $1 }
  | LPAREN IF exp exp exp RPAREN
    { If ($3, $4, $5) }
  | LPAREN exp list(exp) RPAREN
    { Apply ($2, $3) }
  | LPAREN LAMBDA LPAREN args = list(def_arg) RPAREN COLON t = typ e = exp RPAREN
    {
      let open Core_kernel in
      let s = Hash_set.create (module String) in
      List.iter args ~f:(fun (x, _) ->
          match Hash_set.strict_add s x with
          | Error _ ->
             invalid_arg ("lambda: arg " ^ x ^ " cannot be redefined")
          | Ok () -> ());
      Lambda (args, t, e)
    }
  | LPAREN SETBANG VAR exp RPAREN
    { Setbang ($3, $4) }
  | LPAREN BEGIN nonempty_list(exp) RPAREN
    {
      let open Core_kernel in
      let len = List.length $3 in
      Begin (List.take $3 (len - 1), List.last_exn $3)
    }
  | LPAREN WHEN exp nonempty_list(exp) RPAREN
    { When ($3, $4) }
  | LPAREN UNLESS exp nonempty_list(exp) RPAREN
    { Unless ($3, $4) }
  | LPAREN WHILE exp exp RPAREN
    { While ($3, $4) }

prim:
  | LPAREN READ RPAREN
    { Read }
  | LPAREN PRINT exp RPAREN
    { Print $3 }
  | LPAREN MINUS exp RPAREN
    { Minus $3 }
  | LPAREN PLUS exp exp RPAREN
    { Plus ($3, $4) }
  | LPAREN PLUS exp exp nonempty_list(exp) RPAREN
    { lassoc_prim $3 $4 $5 ~f:(fun e1 e2 -> Plus (e1, e2)) }
  | LPAREN MINUS exp exp RPAREN
    { Subtract ($3, $4) }
  | LPAREN MINUS exp exp nonempty_list(exp) RPAREN
    { lassoc_prim $3 $4 $5 ~f:(fun e1 e2 -> Subtract (e1, e2)) }
  | LPAREN STAR exp exp RPAREN
    { Mult ($3, $4) }
  | LPAREN STAR exp exp nonempty_list(exp) RPAREN
    { lassoc_prim $3 $4 $5 ~f:(fun e1 e2 -> Mult (e1, e2)) }
  | LPAREN FSLASH exp exp RPAREN
    { Div ($3, $4) }
  | LPAREN FSLASH exp exp nonempty_list(exp) RPAREN
    { lassoc_prim $3 $4 $5 ~f:(fun e1 e2 -> Div (e1, e2)) }
  | LPAREN REM exp exp RPAREN
    { Rem ($3, $4) }
  | LPAREN REM exp exp nonempty_list(exp) RPAREN
    { lassoc_prim $3 $4 $5 ~f:(fun e1 e2 -> Rem (e1, e2)) }
  | LPAREN LAND exp exp RPAREN
    { Land ($3, $4) }
  | LPAREN LAND exp exp nonempty_list(exp) RPAREN
    { lassoc_prim $3 $4 $5 ~f:(fun e1 e2 -> Land (e1, e2)) }
  | LPAREN LOR exp exp RPAREN
    { Lor ($3, $4) }
  | LPAREN LOR exp exp nonempty_list(exp) RPAREN
    { lassoc_prim $3 $4 $5 ~f:(fun e1 e2 -> Lor (e1, e2)) }
  | LPAREN LXOR exp exp RPAREN
    { Lxor ($3, $4) }
  | LPAREN LXOR exp exp nonempty_list(exp) RPAREN
    { lassoc_prim $3 $4 $5 ~f:(fun e1 e2 -> Lxor (e1, e2)) }
  | LPAREN LNOT exp RPAREN
    { Lnot $3 }
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
    { rassoc_prim $3 $4 $5 ~f:(fun e1 e2 -> And (e1, e2)) }
  | LPAREN OR exp exp RPAREN
    { Or ($3, $4) }
  | LPAREN OR exp exp nonempty_list(exp) RPAREN
    { rassoc_prim $3 $4 $5 ~f:(fun e1 e2 -> Or (e1, e2)) }
  | LPAREN VECTOR list(exp) RPAREN
    { Vector $3 }
  | LPAREN VECTORLENGTH exp RPAREN
    { Vectorlength $3 }
  | LPAREN VECTORREF exp INT RPAREN
    {
      let open Core_kernel in
      Vectorref ($3, Int64.to_int_exn $4)
    }
  | LPAREN VECTORSETBANG exp INT exp RPAREN
    {
      let open Core_kernel in
      Vectorset ($3, Int64.to_int_exn $4, $5)
    }
  | PROCEDUREARITY exp RPAREN
    { Procedurearity $2 }
      

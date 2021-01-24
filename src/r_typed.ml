open Core_kernel

type var = R.var [@@deriving equal, compare, hash, sexp]

type 'a var_env = 'a String.Map.t

let empty_var_env = String.Map.empty

module Type = R.Type
module Type_map = R.Type_map

let main = "main"

let max_args = 6

type type_env = Type.t var_env

type info = unit

type t = Program of info * def list

and def = Def of var * (var * Type.t) list * Type.t * exp

and exp =
  | Int of Int64.t
  | Bool of bool
  | Void
  | Prim of prim * Type.t
  | Var of var * Type.t
  | Let of var * exp * exp * Type.t
  | If of exp * exp * exp * Type.t
  | Apply of exp * exp list * Type.t
  | Funref of var * Type.t
  | Lambda of (var * Type.t) list * Type.t * exp

and prim =
  | Read
  | Minus of exp
  | Plus of exp * exp
  | Subtract of exp * exp
  | Mult of exp * exp
  | Div of exp * exp
  | Rem of exp * exp
  | Land of exp * exp
  | Lor of exp * exp
  | Lxor of exp * exp
  | Lnot of exp
  | Eq of exp * exp
  | Lt of exp * exp
  | Le of exp * exp
  | Gt of exp * exp
  | Ge of exp * exp
  | Not of exp
  | And of exp * exp
  | Or of exp * exp
  | Vector of exp list
  | Vectorlength of exp
  | Vectorref of exp * int
  | Vectorset of exp * int * exp

let typeof_exp = function
  | Int _ -> Type.Integer
  | Bool _ -> Type.Boolean
  | Void -> Type.Void
  | Prim (_, t) -> t
  | Var (_, t) -> t
  | Let (_, _, _, t) -> t
  | If (_, _, _, t) -> t
  | Apply (_, _, t) -> t
  | Lambda (args, t, _) -> Type.Arrow (List.map args ~f:snd, t)
  | Funref (_, t) -> t

let rec vars_of_exp = function
  | Int _ -> empty_var_env
  | Bool _ -> empty_var_env
  | Void -> empty_var_env
  | Prim (p, _) -> vars_of_prim p
  | Var (v, t) -> String.Map.singleton v t
  | Let (_, e1, e2, _) ->
      let m1 = vars_of_exp e1 in
      let m2 = vars_of_exp e2 in
      Map.merge_skewed m1 m2 ~combine:(fun ~key v _ -> v)
  | If (e1, e2, e3, _) ->
      let m1 = vars_of_exp e1 in
      let m2 = vars_of_exp e2 in
      let m3 = vars_of_exp e3 in
      List.fold [m2; m3] ~init:m1 ~f:(fun acc m ->
          Map.merge_skewed acc m ~combine:(fun ~key v _ -> v))
  | Apply (e, es, _) ->
      let m = vars_of_exp e in
      let ms = List.map es ~f:vars_of_exp in
      List.fold ms ~init:m ~f:(fun acc m ->
          Map.merge_skewed acc m ~combine:(fun ~key v _ -> v))
  | Lambda (_, _, e) -> vars_of_exp e
  | Funref _ -> empty_var_env

and vars_of_prim = function
  | Read -> empty_var_env
  | Minus e | Lnot e | Not e | Vectorlength e | Vectorref (e, _) ->
      vars_of_exp e
  | Plus (e1, e2)
   |Subtract (e1, e2)
   |Mult (e1, e2)
   |Div (e1, e2)
   |Rem (e1, e2)
   |Land (e1, e2)
   |Lor (e1, e2)
   |Lxor (e1, e2)
   |Eq (e1, e2)
   |Lt (e1, e2)
   |Le (e1, e2)
   |Gt (e1, e2)
   |Ge (e1, e2)
   |And (e1, e2)
   |Or (e1, e2)
   |Vectorset (e1, _, e2) ->
      let m1 = vars_of_exp e1 in
      let m2 = vars_of_exp e2 in
      Map.merge_skewed m1 m2 ~combine:(fun ~key v _ -> v)
  | Vector [] -> empty_var_env
  | Vector es ->
      let ms = List.map es ~f:vars_of_exp in
      List.(
        fold (tl_exn ms) ~init:(hd_exn ms) ~f:(fun acc m ->
            Map.merge_skewed acc m ~combine:(fun ~key v _ -> v)))

let main_def = function
  | Program (_, defs) ->
      List.find_exn defs ~f:(function Def (v, _, _, _) -> Label.equal v main)

let rec to_string = function
  | Program (_, defs) ->
      List.map defs ~f:string_of_def |> String.concat ~sep:"\n\n"

and string_of_def = function
  | Def (v, args, t, e) ->
      let s =
        List.map args ~f:(fun (a, t) ->
            Printf.sprintf "[%s : %s]" a (Type.to_string t))
        |> String.concat ~sep:" "
      in
      if String.is_empty s then
        Printf.sprintf "(define (%s) : %s %s)" v (Type.to_string t)
          (string_of_exp e)
      else
        Printf.sprintf "(define (%s %s) : %s %s)" v s (Type.to_string t)
          (string_of_exp e)

and string_of_exp = function
  | Int i -> Int64.to_string i
  | Bool b -> if b then "#t" else "#f"
  | Void -> "(void)"
  | Prim (p, _) -> string_of_prim p
  | Var (v, _) -> v
  | Let (v, e1, e2, _) ->
      Printf.sprintf "(let ([%s %s]) %s)" v (string_of_exp e1)
        (string_of_exp e2)
  | If (e1, e2, e3, _) ->
      Printf.sprintf "(if %s %s %s)" (string_of_exp e1) (string_of_exp e2)
        (string_of_exp e3)
  | Apply (e, [], _) -> Printf.sprintf "(%s)" (string_of_exp e)
  | Apply (e, es, _) ->
      Printf.sprintf "(%s %s)" (string_of_exp e)
        (List.map es ~f:string_of_exp |> String.concat ~sep:" ")
  | Funref (v, _) -> Printf.sprintf "(fun-ref %s)" v
  | Lambda (args, t, e) ->
      let s =
        List.map args ~f:(fun (a, t) ->
            Printf.sprintf "[%s : %s]" a (Type.to_string t))
        |> String.concat ~sep:" "
      in
      Printf.sprintf "(lambda: (%s) : %s %s)" s (Type.to_string t)
        (string_of_exp e)

and string_of_prim = function
  | Read -> "(read)"
  | Minus e -> Printf.sprintf "(- %s)" (string_of_exp e)
  | Plus (e1, e2) ->
      Printf.sprintf "(+ %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Subtract (e1, e2) ->
      Printf.sprintf "(- %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Mult (e1, e2) ->
      Printf.sprintf "(* %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Div (e1, e2) ->
      Printf.sprintf "(/ %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Rem (e1, e2) ->
      Printf.sprintf "(rem %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Land (e1, e2) ->
      Printf.sprintf "(land %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Lor (e1, e2) ->
      Printf.sprintf "(lor %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Lxor (e1, e2) ->
      Printf.sprintf "(lxor %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Lnot e -> Printf.sprintf "(lnot %s)" (string_of_exp e)
  | Eq (e1, e2) ->
      Printf.sprintf "(eq? %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Lt (e1, e2) ->
      Printf.sprintf "(< %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Le (e1, e2) ->
      Printf.sprintf "(<= %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Gt (e1, e2) ->
      Printf.sprintf "(> %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Ge (e1, e2) ->
      Printf.sprintf "(>= %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Not e -> Printf.sprintf "(not %s)" (string_of_exp e)
  | And (e1, e2) ->
      Printf.sprintf "(and %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Or (e1, e2) ->
      Printf.sprintf "(or %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Vector es ->
      let s = List.map es ~f:string_of_exp in
      if List.is_empty s then "(vector)"
      else Printf.sprintf "(vector %s)" (String.concat s ~sep:" ")
  | Vectorlength e -> Printf.sprintf "(vector-length %s)" (string_of_exp e)
  | Vectorref (e, i) ->
      Printf.sprintf "(vector-ref %s %d)" (string_of_exp e) i
  | Vectorset (e1, i, e2) ->
      Printf.sprintf "(vector-set! %s %d %s)" (string_of_exp e1) i
        (string_of_exp e2)

let rec opt = function
  | Program (info, defs) -> Program (info, List.map defs ~f:opt_def)

and opt_def = function
  | Def (v, args, t, e) -> Def (v, args, t, opt_exp empty_var_env e)

and opt_exp env = function
  | Int _ as i -> i
  | Bool _ as b -> b
  | Void -> Void
  | Prim (Read, _) as r -> r
  | Prim (Minus e, t) -> (
    match opt_exp env e with
    | Int i -> Int Int64.(-i)
    | Prim (Minus e, _) -> e
    | e -> Prim (Minus e, t) )
  | Prim (Plus (e1, e2), t) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int i1, Int i2 -> Int Int64.(i1 + i2)
    | Int i1, Prim (Minus (Int i2), _) -> Int Int64.(i1 - i2)
    | Int i1, Prim (Plus (Int i2, e2), _)
     |Prim (Plus (Int i1, e2), _), Int i2 ->
        opt_exp env (Prim (Plus (Int Int64.(i1 + i2), e2), t))
    | Prim (Minus (Int i1), _), Int i2 -> Int Int64.(-i1 + i2)
    | e1, e2 -> Prim (Plus (e1, e2), t) )
  | Prim (Subtract (e1, e2), t) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int i1, Int i2 -> Int Int64.(i1 - i2)
    | Int i1, Prim (Minus (Int i2), _) -> Int Int64.(i1 + i2)
    | Prim (Minus (Int i1), _), Int i2 -> Int Int64.(-i1 - i2)
    | e1, e2 -> Prim (Subtract (e1, e2), t) )
  | Prim (Mult (e1, e2), t) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int 0L, _ -> Int 0L
    | _, Int 0L -> Int 0L
    | Int i1, Int i2 -> Int Int64.(i1 * i2)
    | e1, e2 -> Prim (Mult (e1, e2), t) )
  | Prim (Div (e1, e2), t) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int 0L, _ -> Int 0L
    | _, Int 0L -> failwith "R.opt_exp: divide by zero"
    | Int i1, Int i2 -> Int Int64.(i1 / i2)
    | e1, e2 -> Prim (Div (e1, e2), t) )
  | Prim (Rem (e1, e2), t) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int 0L, _ -> Int 0L
    | _, Int 0L -> failwith "R.opt_exp: divide by zero"
    | Int i1, Int i2 -> Int Int64.(rem i1 i2)
    | e1, e2 -> Prim (Rem (e1, e2), t) )
  | Prim (Land (e1, e2), t) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int 0L, _ -> Int 0L
    | _, Int 0L -> Int 0L
    | Int i1, Int i2 -> Int Int64.(i1 land i2)
    | e1, e2 -> Prim (Land (e1, e2), t) )
  | Prim (Lor (e1, e2), t) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int 0L, e -> e
    | e, Int 0L -> Int 0L
    | Int i1, Int i2 -> Int Int64.(i1 lor i2)
    | e1, e2 -> Prim (Lor (e1, e2), t) )
  | Prim (Lxor (e1, e2), t) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int i1, Int i2 -> Int Int64.(i1 lxor i2)
    | e1, e2 -> Prim (Lxor (e1, e2), t) )
  | Prim (Lnot e, t) -> (
    match opt_exp env e with
    | Int i -> Int Int64.(lnot i)
    | e -> Prim (Lnot e, t) )
  | Prim (Eq (e1, e2), t) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int i1, Int i2 -> Bool Int64.(i1 = i2)
    | Bool b1, Bool b2 -> Bool (Bool.equal b1 b2)
    | e1, e2 -> Prim (Eq (e1, e2), t) )
  | Prim (Lt (e1, e2), t) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int i1, Int i2 -> Bool Int64.(i1 < i2)
    | e1, e2 -> Prim (Lt (e1, e2), t) )
  | Prim (Le (e1, e2), t) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int i1, Int i2 -> Bool Int64.(i1 <= i2)
    | e1, e2 -> Prim (Le (e1, e2), t) )
  | Prim (Gt (e1, e2), t) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int i1, Int i2 -> Bool Int64.(i1 > i2)
    | e1, e2 -> Prim (Gt (e1, e2), t) )
  | Prim (Ge (e1, e2), t) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int i1, Int i2 -> Bool Int64.(i1 >= i2)
    | e1, e2 -> Prim (Ge (e1, e2), t) )
  | Prim (Not e, t) -> (
    match opt_exp env e with
    | Bool b -> Bool (not b)
    | e -> Prim (Not e, t) )
  | Prim (And (e1, e2), t) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Bool b1, Bool b2 -> Bool (b1 && b2)
    | e1, e2 -> Prim (And (e1, e2), t) )
  | Prim (Or (e1, e2), t) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Bool b1, Bool b2 -> Bool (b1 || b2)
    | e1, e2 -> Prim (Or (e1, e2), t) )
  | Prim (Vector es, t) -> Prim (Vector (List.map es ~f:(opt_exp env)), t)
  | Prim (Vectorlength e, t) -> (
    match opt_exp env e with
    | Prim (Vector es, _) -> Int Int64.(List.length es |> of_int)
    | e -> Prim (Vectorlength e, t) )
  | Prim (Vectorref (e, i), t) -> Prim (Vectorref (opt_exp env e, i), t)
  | Prim (Vectorset (e1, i, e2), t) ->
      Prim (Vectorset (opt_exp env e1, i, opt_exp env e2), t)
  | Var (v, _) -> (
    match Map.find env v with
    | None -> failwith ("R.opt_exp: var " ^ v ^ " is not bound")
    | Some e -> e )
  | Let (v, e1, e2, t) ->
      let e1 = opt_exp env e1 in
      let e2 = opt_exp (Map.set env v e1) e2 in
      Let (v, e1, e2, t)
  | If (e1, e2, e3, t) -> (
    match opt_exp env e1 with
    | Bool true -> opt_exp env e2
    | Bool false -> opt_exp env e3
    | e1 -> If (e1, e2, e3, t) )
  | Apply (e, es, t) -> Apply (opt_exp env e, List.map es ~f:(opt_exp env), t)
  | Funref _ as f -> f
  | Lambda _ as l -> l

exception Type_error of string

let typeerr msg = raise (Type_error msg)

let def_prefix = "def_"

let fix_def_name =
  String.map ~f:(function
    | '-' -> '_'
    | c -> c)

let rec type_check = function
  | R.Program (_, defs, exp) ->
      let denv =
        List.map defs ~f:(function R.Def (v, args, t, _) ->
            (v, Type.Arrow (List.map args ~f:snd, t)))
        |> String.Map.of_alist_exn
      in
      let defs = List.map defs ~f:(type_check_def denv) in
      let typ, exp = type_check_exp empty_var_env denv exp in
      let main_def = Def (main, [], typ, exp) in
      Program ((), defs @ [main_def])

and type_check_def denv = function
  | R.Def (v, args, t, e) -> (
      let env = String.Map.of_alist_exn args in
      match type_check_exp env denv e with
      | t', e' when Type.equal t t' ->
          Def (def_prefix ^ fix_def_name v, args, t, e')
      | t', _ ->
          typeerr
            ( "R_typed.type_check_def: def " ^ v ^ " body "
            ^ R.string_of_exp e ^ " has type " ^ Type.to_string t'
            ^ " was declared to have type " ^ Type.to_string t ) )

and type_check_exp env denv = function
  | R.Int i -> (Type.Integer, Int i)
  | R.Bool b -> (Type.Boolean, Bool b)
  | R.Void -> (Type.Void, Void)
  | R.(Prim (Procedurearity e)) -> (
    match type_check_exp env denv e with
    | Type.Arrow (args, _), _ ->
        (Type.Integer, Int (List.length args |> Int64.of_int))
    | t, _ ->
        typeerr
          ( "R_typed.type_check_exp: procedure-arity of " ^ R.string_of_exp e
          ^ " has type " ^ Type.to_string t ^ "; it is not a function" ) )
  | R.Prim p ->
      let t, p = type_check_prim env denv p in
      (t, Prim (p, t))
  | R.Var v -> (
    match Map.find env v with
    | Some t -> (t, Var (v, t))
    | None -> (
      match Map.find denv v with
      | Some t -> (t, Funref (def_prefix ^ fix_def_name v, t))
      | None -> typeerr ("R_typed.type_check_exp: var " ^ v ^ " is not bound")
      ) )
  | R.Let (v, e1, e2) ->
      let t1, e1 = type_check_exp env denv e1 in
      let t2, e2 = type_check_exp (Map.set env v t1) denv e2 in
      (t2, Let (v, e1, e2, t2))
  | R.If (e1, e2, e3) -> (
    match type_check_exp env denv e1 with
    | Type.Boolean, e1' ->
        let t1, e2' = type_check_exp env denv e2 in
        let t2, e3' = type_check_exp env denv e3 in
        if Type.equal t1 t2 then (t1, If (e1', e2', e3', t1))
        else
          typeerr
            ( "R_typed.type_check_exp: if branches " ^ R.string_of_exp e2
            ^ " (" ^ Type.to_string t1 ^ ") and " ^ R.string_of_exp e3 ^ " ("
            ^ Type.to_string t2 ^ ") are not the same type" )
    | t, _ ->
        typeerr
          ( "R_typed.type_check_exp: if condition " ^ R.string_of_exp e1
          ^ " is of type " ^ Type.to_string t
          ^ " but an expression of type Boolean was expected" ) )
  | R.Apply (e, es) -> (
      let t, e' = type_check_exp env denv e in
      let ts, es' = List.map es ~f:(type_check_exp env denv) |> List.unzip in
      match t with
      | Type.Arrow (targs, tret) -> (
        match List.zip ts targs with
        (* TODO: support partial application? *)
        | Unequal_lengths ->
            typeerr
              ( "R_typed.type_check_exp: apply of " ^ R.string_of_exp e
              ^ "; invalid arity" )
        | Ok l ->
            List.iteri l ~f:(fun i (t1, t2) ->
                if not (Type.equal t1 t2) then
                  typeerr
                    ( "R_typed.type_check_exp: apply of " ^ R.string_of_exp e
                    ^ "; argument "
                    ^ R.string_of_exp (List.nth_exn es i)
                    ^ " has type " ^ Type.to_string t2
                    ^ " but an expression of type " ^ Type.to_string t1
                    ^ " was expected" ));
            (tret, Apply (e', es', tret)) )
      | _ ->
          typeerr
            ( "R_typed.type_check_exp: apply of " ^ R.string_of_exp e
            ^ " has type " ^ Type.to_string t
            ^ "; it is not a function and cannot be applied" ) )
  | R.Lambda (args, t, e) -> (
      let env =
        List.fold args ~init:env ~f:(fun env (x, t) -> Map.set env x t)
      in
      match type_check_exp env denv e with
      | t', e' when Type.equal t t' ->
          (Type.Arrow (List.map args ~f:snd, t), Lambda (args, t, e'))
      | t', _ ->
          typeerr
            ( "R_typed.type_check_exp: lambda body " ^ R.string_of_exp e
            ^ " has type " ^ Type.to_string t'
            ^ " but was declared to have type " ^ Type.to_string t ) )

and type_check_prim env denv = function
  | R.Read -> (Type.Integer, Read)
  | R.Minus e -> (
    match type_check_exp env denv e with
    | Type.Integer, e' -> (Type.Integer, Minus e')
    | t, _ ->
        typeerr
          ( "R_typed.type_check_prim: minus exp " ^ R.string_of_exp e
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" ) )
  | R.Plus (e1, e2) -> (
    match (type_check_exp env denv e1, type_check_exp env denv e2) with
    | (Type.Integer, e1'), (Type.Integer, e2') ->
        (Type.Integer, Plus (e1', e2'))
    | (t, _), (Type.Integer, _) ->
        typeerr
          ( "R_typed.type_check_prim: plus exp " ^ R.string_of_exp e1
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (Type.Integer, _), (t, _) ->
        typeerr
          ( "R_typed.type_check_prim: plus exp " ^ R.string_of_exp e2
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (t1, _), (t2, _) ->
        typeerr
          ( "R_typed.type_check_prim: plus exps " ^ R.string_of_exp e1
          ^ " and " ^ R.string_of_exp e2 ^ " have types " ^ Type.to_string t1
          ^ " and " ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | R.Subtract (e1, e2) -> (
    match (type_check_exp env denv e1, type_check_exp env denv e2) with
    | (Type.Integer, e1'), (Type.Integer, e2') ->
        (Type.Integer, Subtract (e1', e2'))
    | (t, _), (Type.Integer, _) ->
        typeerr
          ( "R_typed.type_check_prim: subtract exp " ^ R.string_of_exp e1
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (Type.Integer, _), (t, _) ->
        typeerr
          ( "R_typed.type_check_prim: subtract exp " ^ R.string_of_exp e2
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (t1, _), (t2, _) ->
        typeerr
          ( "R_typed.type_check_prim: subtract exps " ^ R.string_of_exp e1
          ^ " and " ^ R.string_of_exp e2 ^ " have types " ^ Type.to_string t1
          ^ " and " ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | R.Mult (e1, e2) -> (
    match (type_check_exp env denv e1, type_check_exp env denv e2) with
    | (Type.Integer, e1'), (Type.Integer, e2') ->
        (Type.Integer, Mult (e1', e2'))
    | (t, _), (Type.Integer, _) ->
        typeerr
          ( "R_typed.type_check_prim: mult exp " ^ R.string_of_exp e1
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (Type.Integer, _), (t, _) ->
        typeerr
          ( "R_typed.type_check_prim: mult exp " ^ R.string_of_exp e2
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (t1, _), (t2, _) ->
        typeerr
          ( "R_typed.type_check_prim: mult exps " ^ R.string_of_exp e1
          ^ " and " ^ R.string_of_exp e2 ^ " have types " ^ Type.to_string t1
          ^ " and " ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | R.Div (e1, e2) -> (
    match (type_check_exp env denv e1, type_check_exp env denv e2) with
    | (Type.Integer, e1'), (Type.Integer, e2') ->
        (Type.Integer, Div (e1', e2'))
    | (t, _), (Type.Integer, _) ->
        typeerr
          ( "R_typed.type_check_prim: div exp " ^ R.string_of_exp e1
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (Type.Integer, _), (t, _) ->
        typeerr
          ( "R_typed.type_check_prim: div exp " ^ R.string_of_exp e2
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (t1, _), (t2, _) ->
        typeerr
          ( "R_typed.type_check_prim: div exps " ^ R.string_of_exp e1
          ^ " and " ^ R.string_of_exp e2 ^ " have types " ^ Type.to_string t1
          ^ " and " ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | R.Rem (e1, e2) -> (
    match (type_check_exp env denv e1, type_check_exp env denv e2) with
    | (Type.Integer, e1'), (Type.Integer, e2') ->
        (Type.Integer, Rem (e1', e2'))
    | (t, _), (Type.Integer, _) ->
        typeerr
          ( "R_typed.type_check_prim: rem exp " ^ R.string_of_exp e1
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (Type.Integer, _), (t, _) ->
        typeerr
          ( "R_typed.type_check_prim: rem exp " ^ R.string_of_exp e2
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (t1, _), (t2, _) ->
        typeerr
          ( "R_typed.type_check_prim: rem exps " ^ R.string_of_exp e1
          ^ " and " ^ R.string_of_exp e2 ^ " have types " ^ Type.to_string t1
          ^ " and " ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | R.Land (e1, e2) -> (
    match (type_check_exp env denv e1, type_check_exp env denv e2) with
    | (Type.Integer, e1'), (Type.Integer, e2') ->
        (Type.Integer, Land (e1', e2'))
    | (t, _), (Type.Integer, _) ->
        typeerr
          ( "R_typed.type_check_prim: land exp " ^ R.string_of_exp e1
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (Type.Integer, _), (t, _) ->
        typeerr
          ( "R_typed.type_check_prim: land exp " ^ R.string_of_exp e2
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (t1, _), (t2, _) ->
        typeerr
          ( "R_typed.type_check_prim: land exps " ^ R.string_of_exp e1
          ^ " and " ^ R.string_of_exp e2 ^ " have types " ^ Type.to_string t1
          ^ " and " ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | R.Lor (e1, e2) -> (
    match (type_check_exp env denv e1, type_check_exp env denv e2) with
    | (Type.Integer, e1'), (Type.Integer, e2') ->
        (Type.Integer, Lor (e1', e2'))
    | (t, _), (Type.Integer, _) ->
        typeerr
          ( "R_typed.type_check_prim: lor exp " ^ R.string_of_exp e1
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (Type.Integer, _), (t, _) ->
        typeerr
          ( "R_typed.type_check_prim: lor exp " ^ R.string_of_exp e2
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (t1, _), (t2, _) ->
        typeerr
          ( "R_typed.type_check_prim: lor exps " ^ R.string_of_exp e1
          ^ " and " ^ R.string_of_exp e2 ^ " have types " ^ Type.to_string t1
          ^ " and " ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | R.Lxor (e1, e2) -> (
    match (type_check_exp env denv e1, type_check_exp env denv e2) with
    | (Type.Integer, e1'), (Type.Integer, e2') ->
        (Type.Integer, Lxor (e1', e2'))
    | (t, _), (Type.Integer, _) ->
        typeerr
          ( "R_typed.type_check_prim: lxor exp " ^ R.string_of_exp e1
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (Type.Integer, _), (t, _) ->
        typeerr
          ( "R_typed.type_check_prim: lxor exp " ^ R.string_of_exp e2
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (t1, _), (t2, _) ->
        typeerr
          ( "R_typed.type_check_prim: lxor exps " ^ R.string_of_exp e1
          ^ " and " ^ R.string_of_exp e2 ^ " have types " ^ Type.to_string t1
          ^ " and " ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | R.Lnot e -> (
    match type_check_exp env denv e with
    | Type.Integer, e' -> (Type.Integer, Lnot e')
    | t, _ ->
        typeerr
          ( "R_typed.type_check_prim: lnot exp " ^ R.string_of_exp e
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" ) )
  | R.Eq (e1, e2) ->
      let t1, e1' = type_check_exp env denv e1 in
      let t2, e2' = type_check_exp env denv e2 in
      if Type.equal t1 t2 then (Type.Boolean, Eq (e1', e2'))
      else
        typeerr
          ( "R_typed.type_check_prim: eq? exps " ^ R.string_of_exp e1 ^ " ("
          ^ Type.to_string t1 ^ ") and " ^ R.string_of_exp e2 ^ " ("
          ^ Type.to_string t2 ^ ") are not the same type" )
  | R.Lt (e1, e2) -> (
    match (type_check_exp env denv e1, type_check_exp env denv e2) with
    | (Type.Integer, e1'), (Type.Integer, e2') ->
        (Type.Boolean, Lt (e1', e2'))
    | (t, _), (Type.Integer, _) ->
        typeerr
          ( "R_typed.type_check_prim: < exp " ^ R.string_of_exp e1
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (Type.Integer, _), (t, _) ->
        typeerr
          ( "R_typed.type_check_prim: < exp " ^ R.string_of_exp e2
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (t1, _), (t2, _) ->
        typeerr
          ( "R_typed.type_check_prim: < exps " ^ R.string_of_exp e1 ^ " and "
          ^ R.string_of_exp e2 ^ " have types " ^ Type.to_string t1 ^ " and "
          ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | R.Le (e1, e2) -> (
    match (type_check_exp env denv e1, type_check_exp env denv e2) with
    | (Type.Integer, e1'), (Type.Integer, e2') ->
        (Type.Boolean, Le (e1', e2'))
    | (t, _), (Type.Integer, _) ->
        typeerr
          ( "R_typed.type_check_prim: <= exp " ^ R.string_of_exp e1
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (Type.Integer, _), (t, _) ->
        typeerr
          ( "R_typed.type_check_prim: <= exp " ^ R.string_of_exp e2
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (t1, _), (t2, _) ->
        typeerr
          ( "R_typed.type_check_prim: <= exps " ^ R.string_of_exp e1
          ^ " and " ^ R.string_of_exp e2 ^ " have types " ^ Type.to_string t1
          ^ " and " ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | R.Gt (e1, e2) -> (
    match (type_check_exp env denv e1, type_check_exp env denv e2) with
    | (Type.Integer, e1'), (Type.Integer, e2') ->
        (Type.Boolean, Gt (e1', e2'))
    | (t, _), (Type.Integer, _) ->
        typeerr
          ( "R_typed.type_check_prim: > exp " ^ R.string_of_exp e1
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (Type.Integer, _), (t, _) ->
        typeerr
          ( "R_typed.type_check_prim: > exp " ^ R.string_of_exp e2
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (t1, _), (t2, _) ->
        typeerr
          ( "R_typed.type_check_prim: > exps " ^ R.string_of_exp e1 ^ " and "
          ^ R.string_of_exp e2 ^ " have types " ^ Type.to_string t1 ^ " and "
          ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | R.Ge (e1, e2) -> (
    match (type_check_exp env denv e1, type_check_exp env denv e2) with
    | (Type.Integer, e1'), (Type.Integer, e2') ->
        (Type.Boolean, Ge (e1', e2'))
    | (t, _), (Type.Integer, _) ->
        typeerr
          ( "R_typed.type_check_prim: >= exp " ^ R.string_of_exp e1
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (Type.Integer, _), (t, _) ->
        typeerr
          ( "R_typed.type_check_prim: >= exp " ^ R.string_of_exp e2
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | (t1, _), (t2, _) ->
        typeerr
          ( "R_typed.type_check_prim: >= exps " ^ R.string_of_exp e1
          ^ " and " ^ R.string_of_exp e2 ^ " have types " ^ Type.to_string t1
          ^ " and " ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | R.Not e -> (
    match type_check_exp env denv e with
    | Type.Boolean, e' -> (Type.Boolean, Not e')
    | t, _ ->
        typeerr
          ( "R_typed.type_check_prim: minus exp " ^ R.string_of_exp e
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Boolean was expected" ) )
  | R.And (e1, e2) -> (
    match (type_check_exp env denv e1, type_check_exp env denv e2) with
    | (Type.Boolean, e1'), (Type.Boolean, e2') ->
        (Type.Boolean, And (e1', e2'))
    | (t, _), (Type.Boolean, _) ->
        typeerr
          ( "R_typed.type_check_prim: and exp " ^ R.string_of_exp e1
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Boolean was expected" )
    | (Type.Boolean, _), (t, _) ->
        typeerr
          ( "R_typed.type_check_prim: and exp " ^ R.string_of_exp e2
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Boolean was expected" )
    | (t1, _), (t2, _) ->
        typeerr
          ( "R_typed.type_check_prim: and exps " ^ R.string_of_exp e1
          ^ " and " ^ R.string_of_exp e2 ^ " have types " ^ Type.to_string t1
          ^ " and " ^ Type.to_string t2
          ^ " but expressions of type Boolean were expected" ) )
  | R.Or (e1, e2) -> (
    match (type_check_exp env denv e1, type_check_exp env denv e2) with
    | (Type.Boolean, e1'), (Type.Boolean, e2') ->
        (Type.Boolean, Or (e1', e2'))
    | (t, _), (Type.Boolean, _) ->
        typeerr
          ( "R_typed.type_check_prim: or exp " ^ R.string_of_exp e1
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Boolean was expected" )
    | (Type.Boolean, _), (t, _) ->
        typeerr
          ( "R_typed.type_check_prim: or exp " ^ R.string_of_exp e2
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Boolean was expected" )
    | (t1, _), (t2, _) ->
        typeerr
          ( "R_typed.type_check_prim: or exps " ^ R.string_of_exp e1
          ^ " and " ^ R.string_of_exp e2 ^ " have types " ^ Type.to_string t1
          ^ " and " ^ Type.to_string t2
          ^ " but expressions of type Boolean were expected" ) )
  | R.Vector es ->
      let ts, es' = List.map es ~f:(type_check_exp env denv) |> List.unzip in
      (Type.Vector ts, Vector es')
  | R.Vectorlength e -> (
    match type_check_exp env denv e with
    | Type.Vector _, e' -> (Type.Integer, Vectorlength e')
    | t, _ ->
        typeerr
          ( "R_typed.type_check_prim: vector-length of " ^ R.string_of_exp e
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Vector was expected" ) )
  | R.Vectorref (e, i) -> (
    match type_check_exp env denv e with
    | Type.Vector ts, e' -> (
      match List.nth ts i with
      | Some t -> (t, Vectorref (e', i))
      | None ->
          typeerr
            ( "R_typed.type_check_prim: vector-ref of " ^ R.string_of_exp e
            ^ ", index " ^ Int.to_string i ^ " out of bounds" ) )
    | t, _ ->
        typeerr
          ( "R_typed.type_check_prim: vector-ref of " ^ R.string_of_exp e
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Vector was expected" ) )
  | R.Vectorset (e1, i, e2) -> (
    match (type_check_exp env denv e1, type_check_exp env denv e2) with
    | (Type.Vector ts, e1'), (t2, e2') -> (
      match List.nth ts i with
      | None ->
          typeerr
            ( "R_typed.type_check_prim: vector-set of " ^ R.string_of_exp e1
            ^ ", index " ^ Int.to_string i ^ " out of bounds" )
      | Some t ->
          if Type.equal t t2 then (Type.Void, Vectorset (e1', i, e2'))
          else
            typeerr
              ( "R_typed.type_check_prim: vector-set of "
              ^ R.string_of_exp e1 ^ " at index " ^ Int.to_string i
              ^ ", expression " ^ R.string_of_exp e2 ^ " has type "
              ^ Type.to_string t2 ^ " but an expression of type "
              ^ Type.to_string t ^ " was expected" ) )
    | (t, _), _ ->
        typeerr
          ( "R_typed.type_check_prim: vector-set of " ^ R.string_of_exp e1
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Vector was expected" ) )
  (* this is handled above since it returns an expression *)
  | R.Procedurearity _ -> assert false

let read_int () =
  Out_channel.(flush stdout);
  Int64.of_string In_channel.(input_line_exn stdin)

type answer =
  [ `Int of Int64.t
  | `Bool of bool
  | `Void
  | `Vector of answer array
  | `Def of var
  | `Function of answer var_env * var list * exp ]

let rec string_of_answer ?(nested = false) = function
  | `Int i -> Int64.to_string i
  | `Bool false -> "#f"
  | `Bool true -> "#t"
  | `Void -> "#<void>"
  | `Vector as' ->
      let s =
        Printf.sprintf "#(%s)"
          ( Array.map as' ~f:(string_of_answer ~nested:true)
          |> Array.to_list |> String.concat ~sep:" " )
      in
      if nested then s else "'" ^ s
  | `Def _ -> "#<function>"
  | `Function _ -> "#<function>"

let rec interp ?(read = None) = function
  | Program (_, defs) as p ->
      let (Def (_, _, _, e)) = main_def p in
      interp_exp empty_var_env defs e ~read

and interp_exp ?(read = None) env defs = function
  | Int i -> `Int i
  | Bool b -> `Bool b
  | Void -> `Void
  | Prim (p, _) -> interp_prim env defs p ~read
  | Var (v, _) -> (
    match Map.find env v with
    | None -> failwith ("R.interp_exp: var " ^ v ^ " is not bound")
    | Some e -> e )
  | Let (v, e1, e2, _) ->
      let e1 = interp_exp env defs e1 ~read in
      interp_exp (Map.set env v e1) defs e2 ~read
  | If (e1, e2, e3, _) -> (
    match interp_exp env defs e1 ~read with
    | `Bool true -> interp_exp env defs e2 ~read
    | `Bool false -> interp_exp env defs e3 ~read
    | _ -> assert false )
  | Apply (e, es, _) -> (
    match interp_exp env defs e ~read with
    | `Def v ->
        let (Def (_, args, _, e')) =
          List.find_exn defs ~f:(function Def (v', _, _, _) ->
              Label.equal v v')
        in
        let args = List.map args ~f:fst in
        let es = List.map es ~f:(interp_exp env defs ~read) in
        let env = List.zip_exn args es |> String.Map.of_alist_exn in
        interp_exp env defs e' ~read
    | `Function (env', args, e') ->
        let es = List.map es ~f:(interp_exp env defs ~read) in
        let env =
          List.zip_exn args es
          |> List.fold ~init:env' ~f:(fun acc (x, e) -> Map.set acc x e)
        in
        interp_exp env defs e' ~read
    | _ -> assert false )
  | Funref (v, _) -> `Def v
  | Lambda (args, _, e) -> `Function (env, List.map args ~f:fst, e)

and interp_prim ?(read = None) env defs = function
  | Read -> (
    match read with
    | Some i -> `Int i
    | None -> `Int (read_int ()) )
  | Minus e -> (
    match interp_exp env defs e ~read with
    | `Int i -> `Int Int64.(-i)
    | _ -> assert false )
  | Plus (e1, e2) -> (
      let a1 = interp_exp env defs e1 ~read in
      let a2 = interp_exp env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int Int64.(i1 + i2)
      | _ -> assert false )
  | Subtract (e1, e2) -> (
      let a1 = interp_exp env defs e1 ~read in
      let a2 = interp_exp env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int Int64.(i1 - i2)
      | _ -> assert false )
  | Mult (e1, e2) -> (
      let a1 = interp_exp env defs e1 ~read in
      let a2 = interp_exp env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int Int64.(i1 * i2)
      | _ -> assert false )
  | Div (e1, e2) -> (
      let a1 = interp_exp env defs e1 ~read in
      let a2 = interp_exp env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int Int64.(i1 / i2)
      | _ -> assert false )
  | Rem (e1, e2) -> (
      let a1 = interp_exp env defs e1 ~read in
      let a2 = interp_exp env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int Int64.(rem i1 i2)
      | _ -> assert false )
  | Land (e1, e2) -> (
      let a1 = interp_exp env defs e1 ~read in
      let a2 = interp_exp env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int Int64.(i1 land i2)
      | _ -> assert false )
  | Lor (e1, e2) -> (
      let a1 = interp_exp env defs e1 ~read in
      let a2 = interp_exp env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int Int64.(i1 lor i2)
      | _ -> assert false )
  | Lxor (e1, e2) -> (
      let a1 = interp_exp env defs e1 ~read in
      let a2 = interp_exp env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int Int64.(i1 lxor i2)
      | _ -> assert false )
  | Lnot e -> (
    match interp_exp env defs e ~read with
    | `Int i -> `Int Int64.(lnot i)
    | _ -> assert false )
  | Eq (e1, e2) -> (
      let a1 = interp_exp env defs e1 ~read in
      let a2 = interp_exp env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Bool Int64.(i1 = i2)
      | `Bool b1, `Bool b2 -> `Bool (Bool.equal b1 b2)
      | _ -> assert false )
  | Lt (e1, e2) -> (
      let a1 = interp_exp env defs e1 ~read in
      let a2 = interp_exp env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Bool Int64.(i1 < i2)
      | _ -> assert false )
  | Le (e1, e2) -> (
      let a1 = interp_exp env defs e1 ~read in
      let a2 = interp_exp env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Bool Int64.(i1 <= i2)
      | _ -> assert false )
  | Gt (e1, e2) -> (
      let a1 = interp_exp env defs e1 ~read in
      let a2 = interp_exp env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Bool Int64.(i1 > i2)
      | _ -> assert false )
  | Ge (e1, e2) -> (
      let a1 = interp_exp env defs e1 ~read in
      let a2 = interp_exp env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Bool Int64.(i1 >= i2)
      | _ -> assert false )
  | Not e -> (
    match interp_exp env defs e ~read with
    | `Bool b -> `Bool (not b)
    | _ -> assert false )
  | And (e1, e2) -> (
      let a1 = interp_exp env defs e1 ~read in
      let a2 = interp_exp env defs e2 ~read in
      match (a1, a2) with
      | `Bool b1, `Bool b2 -> `Bool (b1 && b2)
      | _ -> assert false )
  | Or (e1, e2) -> (
      let a1 = interp_exp env defs e1 ~read in
      let a2 = interp_exp env defs e2 ~read in
      match (a1, a2) with
      | `Bool b1, `Bool b2 -> `Bool (b1 || b2)
      | _ -> assert false )
  | Vector es ->
      `Vector (List.map es ~f:(interp_exp env defs ~read) |> Array.of_list)
  | Vectorlength e -> (
    match interp_exp env defs e ~read with
    | `Vector as' -> `Int Int64.(Array.length as' |> of_int)
    | _ -> assert false )
  | Vectorref (e, i) -> (
    match interp_exp env defs e ~read with
    | `Vector as' -> as'.(i)
    | _ -> assert false )
  | Vectorset (e1, i, e2) -> (
      let a1 = interp_exp env defs e1 ~read in
      let a2 = interp_exp env defs e2 ~read in
      match a1 with
      | `Vector as' -> as'.(i) <- a2; `Void
      | _ -> assert false )

let rec uniquify = function
  | Program (info, defs) -> Program (info, List.map defs ~f:uniquify_def)

and uniquify_def = function
  | Def (v, args, t, e) ->
      let m, args =
        let n = 1 in
        List.fold args ~init:(empty_var_env, []) ~f:(fun (m, args) (x, t) ->
            (Map.set m x n, (newvar x n, t) :: args))
      in
      let _, e = uniquify_exp m e in
      Def (v, List.rev args, t, e)

and uniquify_exp m = function
  | Int _ as i -> (m, i)
  | Bool _ as b -> (m, b)
  | Void -> (m, Void)
  | Prim (p, t) ->
      let _, p = uniquify_prim m p in
      (m, Prim (p, t))
  | Var (v, t) -> (
    match Map.find m v with
    | None -> failwith ("R.uniquify_exp: var " ^ v ^ " is not bound")
    | Some n -> (m, Var (newvar v n, t)) )
  | Let (v, e1, e2, t) ->
      let n =
        match Map.find m v with
        | None -> 1
        | Some n -> n + 1
      in
      let m' = Map.set m v n in
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m' e2 in
      (m, Let (newvar v n, e1, e2, t))
  | If (e1, e2, e3, t) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      let _, e3 = uniquify_exp m e3 in
      (m, If (e1, e2, e3, t))
  | Apply (e, es, t) ->
      let _, e = uniquify_exp m e in
      let _, es = List.map es ~f:(uniquify_exp m) |> List.unzip in
      (m, Apply (e, es, t))
  | Funref _ as f -> (m, f)
  | Lambda (args, t, e) ->
      let m', args =
        List.fold args ~init:(m, []) ~f:(fun (m, args) (x, t) ->
            let n =
              match Map.find m x with
              | None -> 1
              | Some n -> n + 1
            in
            (Map.set m x n, (newvar x n, t) :: args))
      in
      let _, e = uniquify_exp m' e in
      (m, Lambda (List.rev args, t, e))

and uniquify_prim m = function
  | Read -> (m, Read)
  | Minus e ->
      let _, e = uniquify_exp m e in
      (m, Minus e)
  | Plus (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Plus (e1, e2))
  | Subtract (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Subtract (e1, e2))
  | Mult (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Mult (e1, e2))
  | Div (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Div (e1, e2))
  | Rem (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Rem (e1, e2))
  | Land (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Land (e1, e2))
  | Lor (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Lor (e1, e2))
  | Lxor (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Lxor (e1, e2))
  | Lnot e ->
      let _, e = uniquify_exp m e in
      (m, Lnot e)
  | Eq (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Eq (e1, e2))
  | Lt (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Lt (e1, e2))
  | Le (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Le (e1, e2))
  | Gt (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Gt (e1, e2))
  | Ge (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Ge (e1, e2))
  | Not e ->
      let _, e = uniquify_exp m e in
      (m, Not e)
  | And (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, And (e1, e2))
  | Or (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Or (e1, e2))
  | Vector es ->
      let _, es = List.map es ~f:(uniquify_exp m) |> List.unzip in
      (m, Vector es)
  | Vectorlength e ->
      let _, e = uniquify_exp m e in
      (m, Vectorlength e)
  | Vectorref (e, i) ->
      let _, e = uniquify_exp m e in
      (m, Vectorref (e, i))
  | Vectorset (e1, i, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Vectorset (e1, i, e2))

and newvar v n = Printf.sprintf "%s.%d" v n

let rec convert_to_closures = function
  | Program (info, defs) ->
      let n = ref 0 in
      let defs, new_defs =
        List.(map defs ~f:(convert_to_closures_def n) |> unzip)
      in
      let defs = List.concat new_defs @ defs in
      Program (info, List.map defs ~f:(fix_types_def defs))

and fix_types_def defs = function
  | Def (v, args, t, e) ->
      Def (v, args, t, fix_types_exp defs (String.Map.of_alist_exn args) e)

and fix_types_exp defs env = function
  | Int _ as i -> i
  | Bool _ as b -> b
  | Void -> Void
  | Prim (p, t) ->
      let p, t = fix_types_prim defs env p in
      Prim (p, t)
  | Var (v, t) -> (
    match Map.find env v with
    | None -> assert false
    | Some t -> Var (v, t) )
  | Let (v, e1, e2, t) ->
      let e1 = fix_types_exp defs env e1 in
      let t1 = typeof_exp e1 in
      let e2 = fix_types_exp defs (Map.set env v t1) e2 in
      let t = typeof_exp e2 in
      Let (v, e1, e2, t)
  | If (e1, e2, e3, t) ->
      let e1 = fix_types_exp defs env e1 in
      let e2 = fix_types_exp defs env e2 in
      let e3 = fix_types_exp defs env e3 in
      (* we already passed the type-checker, but
       * `e2` and `e3` may contain different closure
       * environments. for now we just pick one of them.
       * (there should be a better way to do this...) *)
      let t2 = typeof_exp e2 in
      If (e1, e2, e3, t2)
  | Apply (e, es, t) -> (
      let e = fix_types_exp defs env e in
      let es = List.map es ~f:(fix_types_exp defs env) in
      match typeof_exp e with
      | Type.Arrow (_, tret) -> Apply (e, es, tret)
      | _ -> assert false )
  | Funref (v, t) ->
      (* now that the top-level function has the correct type,
       * we need to update the type in the reference to it. *)
      let (Def (_, args, t, _)) =
        List.find_exn defs ~f:(function Def (v', _, _, _) ->
            String.equal v v')
      in
      let t = Type.Arrow (List.map args ~f:snd, t) in
      Funref (v, t)
  | Lambda _ -> assert false

and fix_types_prim defs env = function
  | Read -> (Read, Type.Integer)
  | Minus e -> (Minus (fix_types_exp defs env e), Type.Integer)
  | Plus (e1, e2) ->
      ( Plus (fix_types_exp defs env e1, fix_types_exp defs env e2)
      , Type.Integer )
  | Subtract (e1, e2) ->
      ( Subtract (fix_types_exp defs env e1, fix_types_exp defs env e2)
      , Type.Integer )
  | Mult (e1, e2) ->
      ( Mult (fix_types_exp defs env e1, fix_types_exp defs env e2)
      , Type.Integer )
  | Div (e1, e2) ->
      ( Div (fix_types_exp defs env e1, fix_types_exp defs env e2)
      , Type.Integer )
  | Rem (e1, e2) ->
      ( Rem (fix_types_exp defs env e1, fix_types_exp defs env e2)
      , Type.Integer )
  | Land (e1, e2) ->
      ( Land (fix_types_exp defs env e1, fix_types_exp defs env e2)
      , Type.Integer )
  | Lor (e1, e2) ->
      ( Lor (fix_types_exp defs env e1, fix_types_exp defs env e2)
      , Type.Integer )
  | Lxor (e1, e2) ->
      ( Lxor (fix_types_exp defs env e1, fix_types_exp defs env e2)
      , Type.Integer )
  | Lnot e -> (Lnot (fix_types_exp defs env e), Type.Integer)
  | Eq (e1, e2) ->
      ( Eq (fix_types_exp defs env e1, fix_types_exp defs env e2)
      , Type.Boolean )
  | Lt (e1, e2) ->
      ( Lt (fix_types_exp defs env e1, fix_types_exp defs env e2)
      , Type.Boolean )
  | Le (e1, e2) ->
      ( Le (fix_types_exp defs env e1, fix_types_exp defs env e2)
      , Type.Boolean )
  | Gt (e1, e2) ->
      ( Gt (fix_types_exp defs env e1, fix_types_exp defs env e2)
      , Type.Boolean )
  | Ge (e1, e2) ->
      ( Ge (fix_types_exp defs env e1, fix_types_exp defs env e2)
      , Type.Boolean )
  | Not e -> (Not (fix_types_exp defs env e), Type.Boolean)
  | And (e1, e2) ->
      ( And (fix_types_exp defs env e1, fix_types_exp defs env e2)
      , Type.Boolean )
  | Or (e1, e2) ->
      ( Or (fix_types_exp defs env e1, fix_types_exp defs env e2)
      , Type.Boolean )
  | Vector es ->
      let es = List.map es ~f:(fix_types_exp defs env) in
      let ts = List.map es ~f:typeof_exp in
      (Vector es, Type.Vector ts)
  | Vectorlength e ->
      let e = fix_types_exp defs env e in
      (Vectorlength e, Type.Integer)
  | Vectorref (e, i) -> (
      let e = fix_types_exp defs env e in
      match typeof_exp e with
      | Type.Vector ts -> (Vectorref (e, i), List.nth_exn ts i)
      | _ -> assert false )
  | Vectorset (e1, i, e2) ->
      let e1 = fix_types_exp defs env e1 in
      let e2 = fix_types_exp defs env e2 in
      (Vectorset (e1, i, e2), Type.Void)

and convert_to_closures_def n = function
  | Def (v, args, t, e) ->
      let args =
        (* if we encounter a function as an argument, then
         * it's not possible to know the type of the closure
         * environment, so we need to use a dumb "trust me" type *)
        List.map args ~f:(fun (x, t) ->
            match t with
            | Type.Arrow (targs, tret) ->
                (x, Type.(Vector [Arrow (targs, tret); Vector [Trustme]]))
            | _ -> (x, t))
      in
      let e', new_defs =
        convert_to_closures_exp (String.Map.of_alist_exn args) n e
      in
      let args =
        if Label.equal v main then args else ("_", Type.Vector []) :: args
      in
      let t = typeof_exp e' in
      (Def (v, args, t, e'), new_defs)

(* IMPORTANT: the types in the AST will be partly junk
 * after running this, so it's our job to fix them *)
and convert_to_closures_exp env n = function
  | Int _ as i -> (i, [])
  | Bool _ as b -> (b, [])
  | Void -> (Void, [])
  | Prim (p, t) ->
      let p, new_defs = convert_to_closures_prim env n p in
      (Prim (p, t), new_defs)
  | Var (v, t) ->
      let t =
        match Map.find env v with
        | None -> t
        | Some t -> t
      in
      (Var (v, t), [])
  | Let (v, e1, e2, t) ->
      let e1, new_defs1 = convert_to_closures_exp env n e1 in
      let e2, new_defs2 =
        convert_to_closures_exp (Map.set env v (typeof_exp e1)) n e2
      in
      (Let (v, e1, e2, typeof_exp e2), new_defs1 @ new_defs2)
  | If (e1, e2, e3, t) ->
      let e1, new_defs1 = convert_to_closures_exp env n e1 in
      let e2, new_defs2 = convert_to_closures_exp env n e2 in
      let e3, new_defs3 = convert_to_closures_exp env n e3 in
      (If (e1, e2, e3, t), new_defs1 @ new_defs2 @ new_defs3)
  | Apply (e, es, t) ->
      let e, new_defs = convert_to_closures_exp env n e in
      let es, new_defs_es =
        List.(map es ~f:(convert_to_closures_exp env n) |> unzip)
      in
      let et = typeof_exp e in
      let e' =
        Let
          ( "_f"
          , e
          , Apply
              ( Prim (Vectorref (Var ("_f", et), 0), Type.Trustme)
              , Prim (Vectorref (Var ("_f", et), 1), Type.Trustme) :: es
              , t )
          , t )
      in
      (e', new_defs @ List.concat new_defs_es)
  | Funref (v, t) ->
      ( Prim
          ( Vector [Funref (v, t); Prim (Vector [], Type.Vector [])]
          , Type.(Vector [t; Vector []]) )
      , [] )
  | Lambda (args, t, e) ->
      let vars = vars_of_exp e in
      let free_vars =
        List.fold args ~init:vars ~f:(fun acc (x, _) -> Map.remove acc x)
        |> Map.to_alist
      in
      let c, d = newclo n in
      let ct = Type.Vector (List.map free_vars ~f:snd) in
      let clo_arg = (c, ct) in
      let e, new_defs = convert_to_closures_exp env n e in
      let et = typeof_exp e in
      let new_body, _ =
        List.fold_right free_vars
          ~init:(e, List.length free_vars - 1)
          ~f:(fun (x, t) (e, i) ->
            (Let (x, Prim (Vectorref (Var (c, ct), i), t), e, et), i - 1))
      in
      let ft = Type.Arrow (List.map args ~f:snd, t) in
      let new_defs = Def (d, clo_arg :: args, t, new_body) :: new_defs in
      ( Prim
          ( Vector
              [ Funref (d, ft)
              ; Prim
                  ( Vector (List.map free_vars ~f:(fun (x, t) -> Var (x, t)))
                  , ct ) ]
          , Type.Vector [ft; ct] )
      , new_defs )

and convert_to_closures_prim env n = function
  | Read -> (Read, [])
  | Minus e ->
      let e, new_defs = convert_to_closures_exp env n e in
      (Minus e, new_defs)
  | Plus (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp env n e1 in
      let e2, new_defs2 = convert_to_closures_exp env n e2 in
      (Plus (e1, e2), new_defs1 @ new_defs2)
  | Subtract (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp env n e1 in
      let e2, new_defs2 = convert_to_closures_exp env n e2 in
      (Subtract (e1, e2), new_defs1 @ new_defs2)
  | Mult (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp env n e1 in
      let e2, new_defs2 = convert_to_closures_exp env n e2 in
      (Mult (e1, e2), new_defs1 @ new_defs2)
  | Div (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp env n e1 in
      let e2, new_defs2 = convert_to_closures_exp env n e2 in
      (Div (e1, e2), new_defs1 @ new_defs2)
  | Rem (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp env n e1 in
      let e2, new_defs2 = convert_to_closures_exp env n e2 in
      (Rem (e1, e2), new_defs1 @ new_defs2)
  | Land (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp env n e1 in
      let e2, new_defs2 = convert_to_closures_exp env n e2 in
      (Land (e1, e2), new_defs1 @ new_defs2)
  | Lor (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp env n e1 in
      let e2, new_defs2 = convert_to_closures_exp env n e2 in
      (Lor (e1, e2), new_defs1 @ new_defs2)
  | Lxor (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp env n e1 in
      let e2, new_defs2 = convert_to_closures_exp env n e2 in
      (Lxor (e1, e2), new_defs1 @ new_defs2)
  | Lnot e ->
      let e, new_defs = convert_to_closures_exp env n e in
      (Lnot e, new_defs)
  | Eq (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp env n e1 in
      let e2, new_defs2 = convert_to_closures_exp env n e2 in
      (Eq (e1, e2), new_defs1 @ new_defs2)
  | Lt (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp env n e1 in
      let e2, new_defs2 = convert_to_closures_exp env n e2 in
      (Lt (e1, e2), new_defs1 @ new_defs2)
  | Le (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp env n e1 in
      let e2, new_defs2 = convert_to_closures_exp env n e2 in
      (Le (e1, e2), new_defs1 @ new_defs2)
  | Gt (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp env n e1 in
      let e2, new_defs2 = convert_to_closures_exp env n e2 in
      (Gt (e1, e2), new_defs1 @ new_defs2)
  | Ge (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp env n e1 in
      let e2, new_defs2 = convert_to_closures_exp env n e2 in
      (Ge (e1, e2), new_defs1 @ new_defs2)
  | Not e ->
      let e, new_defs = convert_to_closures_exp env n e in
      (Not e, new_defs)
  | And (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp env n e1 in
      let e2, new_defs2 = convert_to_closures_exp env n e2 in
      (And (e1, e2), new_defs1 @ new_defs2)
  | Or (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp env n e1 in
      let e2, new_defs2 = convert_to_closures_exp env n e2 in
      (Or (e1, e2), new_defs1 @ new_defs2)
  | Vector es ->
      let es, new_defs =
        List.(map es ~f:(convert_to_closures_exp env n) |> unzip)
      in
      (Vector es, List.concat new_defs)
  | Vectorlength e ->
      let e, new_defs = convert_to_closures_exp env n e in
      (Vectorlength e, new_defs)
  | Vectorref (e, i) ->
      let e, new_defs = convert_to_closures_exp env n e in
      (Vectorref (e, i), new_defs)
  | Vectorset (e1, i, e2) ->
      let e1, new_defs1 = convert_to_closures_exp env n e1 in
      let e2, new_defs2 = convert_to_closures_exp env n e2 in
      (Vectorset (e1, i, e2), new_defs1 @ new_defs2)

and newclo n =
  let c = Printf.sprintf "_clo%d" !n in
  let d = Printf.sprintf "_def%d" !n in
  incr n; (c, d)

let rec limit_functions = function
  | Program (info, defs) ->
      let defs = List.map defs ~f:limit_functions_def in
      let defs = List.map defs ~f:(limit_functions_def_exp defs) in
      Program (info, defs)

and limit_functions_def = function
  | Def (v, args, t, e) as def ->
      if List.length args <= max_args then def
      else
        let args1, args2 =
          List.foldi args ~init:([], []) ~f:(fun i (a1, a2) a ->
              if i + 1 < max_args then (a :: a1, a2) else (a1, a :: a2))
        in
        let args1 = List.rev args1 in
        let args2 = List.rev args2 in
        let vec_arg = (".args", Type.Vector (List.map args2 ~f:snd)) in
        let e', _ =
          List.fold_right args2
            ~init:(e, List.length args2 - 1)
            ~f:(fun (x, t') (e, i) ->
              ( Let
                  ( x
                  , Prim (Vectorref (Var (".args", snd vec_arg), i), t')
                  , e
                  , t )
              , i - 1 ))
        in
        Def (v, args1 @ [vec_arg], t, e')

and limit_functions_def_exp defs = function
  | Def (v, args, t, e) -> Def (v, args, t, limit_functions_exp defs e)

and limit_functions_exp defs = function
  | Int _ as i -> i
  | Bool _ as b -> b
  | Void -> Void
  | Prim (p, t) -> Prim (limit_functions_prim defs p, t)
  | Var (v, t) -> (
    match t with
    | Type.Arrow (targs, tret) when List.length targs > max_args ->
        let args1, args2 =
          List.foldi targs ~init:([], []) ~f:(fun i (a1, a2) a ->
              if i + 1 < max_args then (a :: a1, a2) else (a1, a :: a2))
        in
        let args1 = List.rev args1 in
        let args2 = List.rev args2 in
        let final_arg_t = Type.Vector args2 in
        Var (v, Type.Arrow (args1 @ [final_arg_t], tret))
    | _ -> Var (v, t) )
  | Let (v, e1, e2, t) ->
      Let (v, limit_functions_exp defs e1, limit_functions_exp defs e2, t)
  | If (e1, e2, e3, t) ->
      If
        ( limit_functions_exp defs e1
        , limit_functions_exp defs e2
        , limit_functions_exp defs e3
        , t )
  | Apply (e, es, t) ->
      let e = limit_functions_exp defs e in
      let es = List.map es ~f:(limit_functions_exp defs) in
      if List.length es <= max_args then Apply (e, es, t)
      else
        let args1, args2 =
          List.foldi es ~init:([], []) ~f:(fun i (a1, a2) a ->
              if i + 1 < max_args then (a :: a1, a2) else (a1, a :: a2))
        in
        let args1 = List.rev args1 in
        let args2 = List.rev args2 in
        let args2_ts = List.map args2 ~f:typeof_exp in
        let final_arg_t = Type.Vector args2_ts in
        let et =
          Type.Arrow (List.map args1 ~f:typeof_exp @ [final_arg_t], t)
        in
        let e =
          match e with
          | Prim (p, _) -> Prim (p, et)
          | Let (v, e1, e2, _) -> Let (v, e1, e2, et)
          | If (e1, e2, e3, _) -> If (e1, e2, e3, et)
          | Apply (e, es, _) -> Apply (e, es, et)
          | Funref _ as f -> f
          | Lambda _ as l -> l
          | _ -> assert false
        in
        Apply (e, args1 @ [Prim (Vector args2, final_arg_t)], t)
  | Funref (v, t) ->
      let (Def (_, args, t', _)) =
        List.find_exn defs ~f:(function Def (v', _, _, _) ->
            Label.equal v v')
      in
      Funref (v, Type.Arrow (List.map args ~f:snd, t'))
  (* convert_to_closures should have erased all lambdas *)
  | Lambda _ -> assert false

and limit_functions_prim defs = function
  | Read -> Read
  | Minus e -> Minus (limit_functions_exp defs e)
  | Plus (e1, e2) ->
      Plus (limit_functions_exp defs e1, limit_functions_exp defs e2)
  | Subtract (e1, e2) ->
      Subtract (limit_functions_exp defs e1, limit_functions_exp defs e2)
  | Mult (e1, e2) ->
      Mult (limit_functions_exp defs e1, limit_functions_exp defs e2)
  | Div (e1, e2) ->
      Div (limit_functions_exp defs e1, limit_functions_exp defs e2)
  | Rem (e1, e2) ->
      Rem (limit_functions_exp defs e1, limit_functions_exp defs e2)
  | Land (e1, e2) ->
      Land (limit_functions_exp defs e1, limit_functions_exp defs e2)
  | Lor (e1, e2) ->
      Lor (limit_functions_exp defs e1, limit_functions_exp defs e2)
  | Lxor (e1, e2) ->
      Lxor (limit_functions_exp defs e1, limit_functions_exp defs e2)
  | Lnot e -> Lnot (limit_functions_exp defs e)
  | Eq (e1, e2) ->
      Eq (limit_functions_exp defs e1, limit_functions_exp defs e2)
  | Lt (e1, e2) ->
      Lt (limit_functions_exp defs e1, limit_functions_exp defs e2)
  | Le (e1, e2) ->
      Le (limit_functions_exp defs e1, limit_functions_exp defs e2)
  | Gt (e1, e2) ->
      Gt (limit_functions_exp defs e1, limit_functions_exp defs e2)
  | Ge (e1, e2) ->
      Ge (limit_functions_exp defs e1, limit_functions_exp defs e2)
  | Not e -> Not (limit_functions_exp defs e)
  | And (e1, e2) ->
      And (limit_functions_exp defs e1, limit_functions_exp defs e2)
  | Or (e1, e2) ->
      Or (limit_functions_exp defs e1, limit_functions_exp defs e2)
  | Vector es -> Vector (List.map es ~f:(limit_functions_exp defs))
  | Vectorlength e -> Vectorlength (limit_functions_exp defs e)
  | Vectorref (e, i) -> Vectorref (limit_functions_exp defs e, i)
  | Vectorset (e1, i, e2) ->
      Vectorset (limit_functions_exp defs e1, i, limit_functions_exp defs e2)

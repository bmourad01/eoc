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
  | Setbang of var * exp
  | Begin of exp list * exp * Type.t
  | While of exp * exp

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
  | Setbang _ -> Type.Void
  | Begin (_, _, t) -> t
  | While _ -> Type.Void

let rec free_vars_of_exp ?(bnd = String.Set.empty) = function
  | Int _ -> empty_var_env
  | Bool _ -> empty_var_env
  | Void -> empty_var_env
  | Prim (p, _) -> free_vars_of_prim p ~bnd
  | Var (v, t) ->
      if Set.mem bnd v then empty_var_env else String.Map.singleton v t
  | Let (x, e1, e2, _) ->
      let m1 = free_vars_of_exp e1 ~bnd in
      let m2 = free_vars_of_exp e2 ~bnd:(Set.add bnd x) in
      Map.merge_skewed m1 m2 ~combine:(fun ~key v _ -> v)
  | If (e1, e2, e3, _) ->
      let m1 = free_vars_of_exp e1 ~bnd in
      let m2 = free_vars_of_exp e2 ~bnd in
      let m3 = free_vars_of_exp e3 ~bnd in
      List.fold [m2; m3] ~init:m1 ~f:(fun acc m ->
          Map.merge_skewed acc m ~combine:(fun ~key v _ -> v))
  | Apply (e, es, _) ->
      let m = free_vars_of_exp e ~bnd in
      let ms = List.map es ~f:(free_vars_of_exp ~bnd) in
      List.fold ms ~init:m ~f:(fun acc m ->
          Map.merge_skewed acc m ~combine:(fun ~key v _ -> v))
  | Lambda (args, _, e) ->
      let bnd =
        List.fold args ~init:bnd ~f:(fun bnd (x, _) -> Set.add bnd x)
      in
      free_vars_of_exp e ~bnd
  | Funref _ -> empty_var_env
  | Setbang (v, e) ->
      let m1 =
        if Set.mem bnd v then empty_var_env
        else String.Map.singleton v (typeof_exp e)
      in
      let m2 = free_vars_of_exp e ~bnd in
      Map.merge_skewed m1 m2 ~combine:(fun ~key v _ -> v)
  | Begin ([], e, _) -> free_vars_of_exp e ~bnd
  | Begin (es, e, _) ->
      let m = free_vars_of_exp e ~bnd in
      let ms = List.map es ~f:(free_vars_of_exp ~bnd) in
      List.fold (m :: List.tl_exn ms) ~init:(List.hd_exn ms) ~f:(fun acc m ->
          Map.merge_skewed acc m ~combine:(fun ~key v _ -> v))
  | While (e1, e2) ->
      let m1 = free_vars_of_exp e1 ~bnd in
      let m2 = free_vars_of_exp e2 ~bnd in
      Map.merge_skewed m1 m2 ~combine:(fun ~key v _ -> v)

and free_vars_of_prim ?(bnd = String.Set.empty) = function
  | Read -> empty_var_env
  | Minus e | Lnot e | Not e | Vectorlength e | Vectorref (e, _) ->
      free_vars_of_exp e ~bnd
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
      let m1 = free_vars_of_exp e1 ~bnd in
      let m2 = free_vars_of_exp e2 ~bnd in
      Map.merge_skewed m1 m2 ~combine:(fun ~key v _ -> v)
  | Vector [] -> empty_var_env
  | Vector es ->
      let ms = List.map es ~f:(free_vars_of_exp ~bnd) in
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
  | Setbang (v, e) -> Printf.sprintf "(set! %s %s)" v (string_of_exp e)
  | Begin ([], e, _) -> Printf.sprintf "(begin %s)" (string_of_exp e)
  | Begin (es, e, _) ->
      Printf.sprintf "(begin %s %s)"
        (List.map es ~f:string_of_exp |> String.concat ~sep:" ")
        (string_of_exp e)
  | While (e1, e2) ->
      Printf.sprintf "(while %s %s)" (string_of_exp e1) (string_of_exp e2)

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
  | Def (v, args, t, e) ->
      let mutated = Hash_set.create (module String) in
      Def (v, args, t, opt_exp mutated empty_var_env e)

and opt_exp mutated env = function
  | Int _ as i -> i
  | Bool _ as b -> b
  | Void -> Void
  | Prim (Read, _) as r -> r
  | Prim (Minus e, t) -> (
    match opt_exp mutated env e with
    | Int i -> Int Int64.(-i)
    | Prim (Minus e, _) -> e
    | e -> Prim (Minus e, t) )
  | Prim (Plus (e1, e2), t) -> (
    match (opt_exp mutated env e1, opt_exp mutated env e2) with
    | Int i1, Int i2 -> Int Int64.(i1 + i2)
    | Int i1, Prim (Minus (Int i2), _) -> Int Int64.(i1 - i2)
    | Int i1, Prim (Plus (Int i2, e2), _)
     |Prim (Plus (Int i1, e2), _), Int i2 ->
        opt_exp mutated env (Prim (Plus (Int Int64.(i1 + i2), e2), t))
    | Prim (Minus (Int i1), _), Int i2 -> Int Int64.(-i1 + i2)
    | e1, e2 -> Prim (Plus (e1, e2), t) )
  | Prim (Subtract (e1, e2), t) -> (
    match (opt_exp mutated env e1, opt_exp mutated env e2) with
    | Int i1, Int i2 -> Int Int64.(i1 - i2)
    | Int i1, Prim (Minus (Int i2), _) -> Int Int64.(i1 + i2)
    | Prim (Minus (Int i1), _), Int i2 -> Int Int64.(-i1 - i2)
    | e1, e2 -> Prim (Subtract (e1, e2), t) )
  | Prim (Mult (e1, e2), t) -> (
    match (opt_exp mutated env e1, opt_exp mutated env e2) with
    | Int 0L, _ -> Int 0L
    | _, Int 0L -> Int 0L
    | Int i1, Int i2 -> Int Int64.(i1 * i2)
    | e1, e2 -> Prim (Mult (e1, e2), t) )
  | Prim (Div (e1, e2), t) -> (
    match (opt_exp mutated env e1, opt_exp mutated env e2) with
    | Int 0L, _ -> Int 0L
    | _, Int 0L -> failwith "R.opt_exp: divide by zero"
    | Int i1, Int i2 -> Int Int64.(i1 / i2)
    | e1, e2 -> Prim (Div (e1, e2), t) )
  | Prim (Rem (e1, e2), t) -> (
    match (opt_exp mutated env e1, opt_exp mutated env e2) with
    | Int 0L, _ -> Int 0L
    | _, Int 0L -> failwith "R.opt_exp: divide by zero"
    | Int i1, Int i2 -> Int Int64.(rem i1 i2)
    | e1, e2 -> Prim (Rem (e1, e2), t) )
  | Prim (Land (e1, e2), t) -> (
    match (opt_exp mutated env e1, opt_exp mutated env e2) with
    | Int 0L, _ -> Int 0L
    | _, Int 0L -> Int 0L
    | Int i1, Int i2 -> Int Int64.(i1 land i2)
    | e1, e2 -> Prim (Land (e1, e2), t) )
  | Prim (Lor (e1, e2), t) -> (
    match (opt_exp mutated env e1, opt_exp mutated env e2) with
    | Int 0L, e -> e
    | e, Int 0L -> Int 0L
    | Int i1, Int i2 -> Int Int64.(i1 lor i2)
    | e1, e2 -> Prim (Lor (e1, e2), t) )
  | Prim (Lxor (e1, e2), t) -> (
    match (opt_exp mutated env e1, opt_exp mutated env e2) with
    | Int i1, Int i2 -> Int Int64.(i1 lxor i2)
    | e1, e2 -> Prim (Lxor (e1, e2), t) )
  | Prim (Lnot e, t) -> (
    match opt_exp mutated env e with
    | Int i -> Int Int64.(lnot i)
    | e -> Prim (Lnot e, t) )
  | Prim (Eq (e1, e2), t) -> (
    match (opt_exp mutated env e1, opt_exp mutated env e2) with
    | Int i1, Int i2 -> Bool Int64.(i1 = i2)
    | Bool b1, Bool b2 -> Bool (Bool.equal b1 b2)
    | e1, e2 -> Prim (Eq (e1, e2), t) )
  | Prim (Lt (e1, e2), t) -> (
    match (opt_exp mutated env e1, opt_exp mutated env e2) with
    | Int i1, Int i2 -> Bool Int64.(i1 < i2)
    | e1, e2 -> Prim (Lt (e1, e2), t) )
  | Prim (Le (e1, e2), t) -> (
    match (opt_exp mutated env e1, opt_exp mutated env e2) with
    | Int i1, Int i2 -> Bool Int64.(i1 <= i2)
    | e1, e2 -> Prim (Le (e1, e2), t) )
  | Prim (Gt (e1, e2), t) -> (
    match (opt_exp mutated env e1, opt_exp mutated env e2) with
    | Int i1, Int i2 -> Bool Int64.(i1 > i2)
    | e1, e2 -> Prim (Gt (e1, e2), t) )
  | Prim (Ge (e1, e2), t) -> (
    match (opt_exp mutated env e1, opt_exp mutated env e2) with
    | Int i1, Int i2 -> Bool Int64.(i1 >= i2)
    | e1, e2 -> Prim (Ge (e1, e2), t) )
  | Prim (Not e, t) -> (
    match opt_exp mutated env e with
    | Bool b -> Bool (not b)
    | e -> Prim (Not e, t) )
  | Prim (And (e1, e2), t) -> (
    match (opt_exp mutated env e1, opt_exp mutated env e2) with
    | Bool false, _ -> Bool false
    | Bool true, e -> opt_exp mutated env e
    | e1, e2 -> Prim (And (e1, e2), t) )
  | Prim (Or (e1, e2), t) -> (
    match (opt_exp mutated env e1, opt_exp mutated env e2) with
    | Bool false, e -> opt_exp mutated env e
    | Bool true, _ -> Bool true
    | e1, e2 -> Prim (Or (e1, e2), t) )
  | Prim (Vector es, t) ->
      Prim (Vector (List.map es ~f:(opt_exp mutated env)), t)
  | Prim (Vectorlength e, t) -> (
    match opt_exp mutated env e with
    | Prim (Vector es, _) -> Int Int64.(List.length es |> of_int)
    | e -> Prim (Vectorlength e, t) )
  | Prim (Vectorref (e, i), t) -> (
    match opt_exp mutated env e with
    | Prim (Vector es, _) -> opt_exp mutated env (List.nth_exn es i)
    | e -> Prim (Vectorref (e, i), t) )
  | Prim (Vectorset (e1, i, e2), t) ->
      Prim (Vectorset (opt_exp mutated env e1, i, opt_exp mutated env e2), t)
  | Var (v, _) as var when Hash_set.mem mutated v -> var
  | Var (v, _) as var -> (
    (* assuming the program has been type-checked,
     * we know that the var is not actually free
     * but we may not have been able to bind it
     * to an expression yet, so leave it as-is. *)
    match Map.find env v with
    | None -> var
    | Some e -> e )
  | Let (v, e1, e2, t) ->
      let e1 = opt_exp mutated env e1 in
      let e2 = opt_exp mutated (Map.set env v e1) e2 in
      Let (v, e1, e2, t)
  | If (e1, e2, e3, t) -> (
    match opt_exp mutated env e1 with
    | Bool true -> opt_exp mutated env e2
    | Bool false -> opt_exp mutated env e3
    | e1 -> If (e1, e2, e3, t) )
  | Apply (e, es, t) ->
      Apply (opt_exp mutated env e, List.map es ~f:(opt_exp mutated env), t)
  | Funref _ as f -> f
  | Lambda (args, t, e) -> Lambda (args, t, opt_exp mutated env e)
  | Setbang (v, e) ->
      let e' = Setbang (v, opt_exp mutated env e) in
      Hash_set.add mutated v; e'
  | Begin (es, e, t) ->
      let es = List.map es ~f:(opt_exp mutated env) in
      let e = opt_exp mutated env e in
      Begin (es, e, t)
  | While _ as w -> w

exception Type_error of string

let typeerr msg = raise (Type_error msg)

let def_prefix = "_def_"

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
  | R.Setbang (v, e) -> (
    match Map.find env v with
    | None ->
        typeerr ("R_typed.type_check_exp: set! var " ^ v ^ " is not bound")
    | Some t -> (
      match type_check_exp env denv e with
      | t', e' when Type.equal t t' -> (Type.Void, Setbang (v, e'))
      | t', _ ->
          typeerr
            ( "R_typed.type_check_exp: set! expression " ^ R.string_of_exp e
            ^ " has type " ^ Type.to_string t'
            ^ " but an expression of type " ^ Type.to_string t
            ^ " was expected" ) ) )
  | R.Begin (es, e) ->
      let ts', es' =
        List.map es ~f:(type_check_exp env denv) |> List.unzip
      in
      List.(
        iter (zip_exn ts' es) ~f:(fun (t, e) ->
            match t with
            | Type.Void -> ()
            | _ ->
                typeerr
                  ( "R_typed.type_check_exp: begin expression "
                  ^ R.string_of_exp e ^ " has type " ^ Type.to_string t
                  ^ " but an expression of type Void was expected" )));
      let t', e' = type_check_exp env denv e in
      (t', Begin (es', e', t'))
  | R.While (e1, e2) -> (
    match type_check_exp env denv e1 with
    | Type.Boolean, e1' -> (
      match type_check_exp env denv e2 with
      | Type.Void, e2' -> (Type.Void, While (e1', e2'))
      | t, _ ->
          typeerr
            ( "R_typed.type_check_exp: while body " ^ R.string_of_exp e1
            ^ " has type " ^ Type.to_string t
            ^ " but an expression of type Void was expected" ) )
    | t, _ ->
        typeerr
          ( "R_typed.type_check_exp: while condition " ^ R.string_of_exp e1
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Boolean was expected" ) )

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
      let menv = Hashtbl.create (module String) in
      interp_exp menv empty_var_env defs e ~read

and interp_exp ?(read = None) menv env defs = function
  | Int i -> `Int i
  | Bool b -> `Bool b
  | Void -> `Void
  | Prim (p, _) -> interp_prim menv env defs p ~read
  | Var (v, _) -> (
    match Hashtbl.find menv v with
    | Some e -> e
    | None -> (
      match Map.find env v with
      | None -> failwith ("R.interp_exp: var " ^ v ^ " is not bound")
      | Some e -> e ) )
  | Let (v, e1, e2, _) ->
      let e1 = interp_exp menv env defs e1 ~read in
      interp_exp menv (Map.set env v e1) defs e2 ~read
  | If (e1, e2, e3, _) -> (
    match interp_exp menv env defs e1 ~read with
    | `Bool true -> interp_exp menv env defs e2 ~read
    | `Bool false -> interp_exp menv env defs e3 ~read
    | _ -> assert false )
  | Apply (e, es, _) -> (
    match interp_exp menv env defs e ~read with
    | `Def v ->
        let (Def (_, args, _, e')) =
          List.find_exn defs ~f:(function Def (v', _, _, _) ->
              Label.equal v v')
        in
        let args = List.map args ~f:fst in
        let es = List.map es ~f:(interp_exp menv env defs ~read) in
        let env = List.zip_exn args es |> String.Map.of_alist_exn in
        interp_exp menv env defs e' ~read
    | `Function (env', args, e') ->
        let es = List.map es ~f:(interp_exp menv env defs ~read) in
        let env =
          List.zip_exn args es
          |> List.fold ~init:env' ~f:(fun acc (x, e) -> Map.set acc x e)
        in
        interp_exp menv env defs e' ~read
    | _ -> assert false )
  | Funref (v, _) -> `Def v
  | Lambda (args, _, e) -> `Function (env, List.map args ~f:fst, e)
  | Setbang (v, e) ->
      let e' = interp_exp menv env defs e ~read in
      Hashtbl.set menv v e'; `Void
  | Begin (es, e, _) ->
      List.iter es ~f:(fun e -> interp_exp menv env defs e ~read |> ignore);
      interp_exp menv env defs e ~read
  | While (e1, e2) ->
      interp_exp menv env defs
        (If (e1, Begin ([e2], While (e1, e2), Type.Void), Void, Type.Void))
        ~read

and interp_prim ?(read = None) menv env defs = function
  | Read -> (
    match read with
    | Some i -> `Int i
    | None -> `Int (read_int ()) )
  | Minus e -> (
    match interp_exp menv env defs e ~read with
    | `Int i -> `Int Int64.(-i)
    | _ -> assert false )
  | Plus (e1, e2) -> (
      let a1 = interp_exp menv env defs e1 ~read in
      let a2 = interp_exp menv env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int Int64.(i1 + i2)
      | _ -> assert false )
  | Subtract (e1, e2) -> (
      let a1 = interp_exp menv env defs e1 ~read in
      let a2 = interp_exp menv env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int Int64.(i1 - i2)
      | _ -> assert false )
  | Mult (e1, e2) -> (
      let a1 = interp_exp menv env defs e1 ~read in
      let a2 = interp_exp menv env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int Int64.(i1 * i2)
      | _ -> assert false )
  | Div (e1, e2) -> (
      let a1 = interp_exp menv env defs e1 ~read in
      let a2 = interp_exp menv env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int Int64.(i1 / i2)
      | _ -> assert false )
  | Rem (e1, e2) -> (
      let a1 = interp_exp menv env defs e1 ~read in
      let a2 = interp_exp menv env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int Int64.(rem i1 i2)
      | _ -> assert false )
  | Land (e1, e2) -> (
      let a1 = interp_exp menv env defs e1 ~read in
      let a2 = interp_exp menv env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int Int64.(i1 land i2)
      | _ -> assert false )
  | Lor (e1, e2) -> (
      let a1 = interp_exp menv env defs e1 ~read in
      let a2 = interp_exp menv env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int Int64.(i1 lor i2)
      | _ -> assert false )
  | Lxor (e1, e2) -> (
      let a1 = interp_exp menv env defs e1 ~read in
      let a2 = interp_exp menv env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int Int64.(i1 lxor i2)
      | _ -> assert false )
  | Lnot e -> (
    match interp_exp menv env defs e ~read with
    | `Int i -> `Int Int64.(lnot i)
    | _ -> assert false )
  | Eq (e1, e2) -> (
      let a1 = interp_exp menv env defs e1 ~read in
      let a2 = interp_exp menv env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Bool Int64.(i1 = i2)
      | `Bool b1, `Bool b2 -> `Bool (Bool.equal b1 b2)
      | _ -> assert false )
  | Lt (e1, e2) -> (
      let a1 = interp_exp menv env defs e1 ~read in
      let a2 = interp_exp menv env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Bool Int64.(i1 < i2)
      | _ -> assert false )
  | Le (e1, e2) -> (
      let a1 = interp_exp menv env defs e1 ~read in
      let a2 = interp_exp menv env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Bool Int64.(i1 <= i2)
      | _ -> assert false )
  | Gt (e1, e2) -> (
      let a1 = interp_exp menv env defs e1 ~read in
      let a2 = interp_exp menv env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Bool Int64.(i1 > i2)
      | _ -> assert false )
  | Ge (e1, e2) -> (
      let a1 = interp_exp menv env defs e1 ~read in
      let a2 = interp_exp menv env defs e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Bool Int64.(i1 >= i2)
      | _ -> assert false )
  | Not e -> (
    match interp_exp menv env defs e ~read with
    | `Bool b -> `Bool (not b)
    | _ -> assert false )
  | And (e1, e2) -> (
      let a1 = interp_exp menv env defs e1 ~read in
      let a2 = interp_exp menv env defs e2 ~read in
      match (a1, a2) with
      | `Bool b1, `Bool b2 -> `Bool (b1 && b2)
      | _ -> assert false )
  | Or (e1, e2) -> (
      let a1 = interp_exp menv env defs e1 ~read in
      let a2 = interp_exp menv env defs e2 ~read in
      match (a1, a2) with
      | `Bool b1, `Bool b2 -> `Bool (b1 || b2)
      | _ -> assert false )
  | Vector es ->
      `Vector
        (List.map es ~f:(interp_exp menv env defs ~read) |> Array.of_list)
  | Vectorlength e -> (
    match interp_exp menv env defs e ~read with
    | `Vector as' -> `Int Int64.(Array.length as' |> of_int)
    | _ -> assert false )
  | Vectorref (e, i) -> (
    match interp_exp menv env defs e ~read with
    | `Vector as' -> as'.(i)
    | _ -> assert false )
  | Vectorset (e1, i, e2) -> (
      let a1 = interp_exp menv env defs e1 ~read in
      let a2 = interp_exp menv env defs e2 ~read in
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
      let m = ref m in
      let e = uniquify_exp m e in
      Def (v, List.rev args, t, e)

and uniquify_exp m = function
  | Int _ as i -> i
  | Bool _ as b -> b
  | Void -> Void
  | Prim (p, t) ->
      let p = uniquify_prim m p in
      Prim (p, t)
  | Var (v, t) -> (
    match Map.find !m v with
    | None -> failwith ("R.uniquify_exp: var " ^ v ^ " is not bound")
    | Some n -> Var (newvar v n, t) )
  | Let (v, e1, e2, t) ->
      let e1 = uniquify_exp m e1 in
      let n =
        match Map.find !m v with
        | None -> 1
        | Some n -> n + 1
      in
      m := Map.set !m v n;
      let e2 = uniquify_exp m e2 in
      Let (newvar v n, e1, e2, t)
  | If (e1, e2, e3, t) ->
      let e1 = uniquify_exp m e1 in
      let e2 = uniquify_exp m e2 in
      let e3 = uniquify_exp m e3 in
      If (e1, e2, e3, t)
  | Apply (e, es, t) ->
      let e = uniquify_exp m e in
      let es = List.map es ~f:(uniquify_exp m) in
      Apply (e, es, t)
  | Funref _ as f -> f
  | Lambda (args, t, e) ->
      let args =
        List.fold args ~init:[] ~f:(fun args (x, t) ->
            let n =
              match Map.find !m x with
              | None -> 1
              | Some n -> n + 1
            in
            m := Map.set !m x n;
            (newvar x n, t) :: args)
      in
      let e = uniquify_exp m e in
      Lambda (List.rev args, t, e)
  | Setbang (v, e) -> (
    match Map.find !m v with
    | None -> failwith ("R.uniquify_exp: var " ^ v ^ " is not bound")
    | Some n ->
        let v = newvar v n in
        let e = uniquify_exp m e in
        Setbang (v, e) )
  | Begin (es, e, t) ->
      let es = List.map es ~f:(uniquify_exp m) in
      let e = uniquify_exp m e in
      Begin (es, e, t)
  | While (e1, e2) ->
      let e1 = uniquify_exp m e1 in
      let e2 = uniquify_exp m e2 in
      While (e1, e2)

and uniquify_prim m = function
  | Read -> Read
  | Minus e ->
      let e = uniquify_exp m e in
      Minus e
  | Plus (e1, e2) ->
      let e1 = uniquify_exp m e1 in
      let e2 = uniquify_exp m e2 in
      Plus (e1, e2)
  | Subtract (e1, e2) ->
      let e1 = uniquify_exp m e1 in
      let e2 = uniquify_exp m e2 in
      Subtract (e1, e2)
  | Mult (e1, e2) ->
      let e1 = uniquify_exp m e1 in
      let e2 = uniquify_exp m e2 in
      Mult (e1, e2)
  | Div (e1, e2) ->
      let e1 = uniquify_exp m e1 in
      let e2 = uniquify_exp m e2 in
      Div (e1, e2)
  | Rem (e1, e2) ->
      let e1 = uniquify_exp m e1 in
      let e2 = uniquify_exp m e2 in
      Rem (e1, e2)
  | Land (e1, e2) ->
      let e1 = uniquify_exp m e1 in
      let e2 = uniquify_exp m e2 in
      Land (e1, e2)
  | Lor (e1, e2) ->
      let e1 = uniquify_exp m e1 in
      let e2 = uniquify_exp m e2 in
      Lor (e1, e2)
  | Lxor (e1, e2) ->
      let e1 = uniquify_exp m e1 in
      let e2 = uniquify_exp m e2 in
      Lxor (e1, e2)
  | Lnot e ->
      let e = uniquify_exp m e in
      Lnot e
  | Eq (e1, e2) ->
      let e1 = uniquify_exp m e1 in
      let e2 = uniquify_exp m e2 in
      Eq (e1, e2)
  | Lt (e1, e2) ->
      let e1 = uniquify_exp m e1 in
      let e2 = uniquify_exp m e2 in
      Lt (e1, e2)
  | Le (e1, e2) ->
      let e1 = uniquify_exp m e1 in
      let e2 = uniquify_exp m e2 in
      Le (e1, e2)
  | Gt (e1, e2) ->
      let e1 = uniquify_exp m e1 in
      let e2 = uniquify_exp m e2 in
      Gt (e1, e2)
  | Ge (e1, e2) ->
      let e1 = uniquify_exp m e1 in
      let e2 = uniquify_exp m e2 in
      Ge (e1, e2)
  | Not e ->
      let e = uniquify_exp m e in
      Not e
  | And (e1, e2) ->
      let e1 = uniquify_exp m e1 in
      let e2 = uniquify_exp m e2 in
      And (e1, e2)
  | Or (e1, e2) ->
      let e1 = uniquify_exp m e1 in
      let e2 = uniquify_exp m e2 in
      Or (e1, e2)
  | Vector es ->
      let es = List.map es ~f:(uniquify_exp m) in
      Vector es
  | Vectorlength e ->
      let e = uniquify_exp m e in
      Vectorlength e
  | Vectorref (e, i) ->
      let e = uniquify_exp m e in
      Vectorref (e, i)
  | Vectorset (e1, i, e2) ->
      let e1 = uniquify_exp m e1 in
      let e2 = uniquify_exp m e2 in
      Vectorset (e1, i, e2)

and newvar v n = Printf.sprintf "%s.%d" v n

let rec escaped_defs = function
  | Program (info, defs) ->
      List.map defs ~f:escaped_defs_def |> List.concat |> String.Set.of_list

and escaped_defs_def = function
  | Def (_, _, _, e) -> escaped_defs_exp e

and escaped_defs_exp = function
  | Int _ -> []
  | Bool _ -> []
  | Void -> []
  | Prim (p, _) -> escaped_defs_prim p
  | Var _ -> []
  | Let (_, e1, e2, _) -> escaped_defs_exp e1 @ escaped_defs_exp e2
  | If (e1, e2, e3, _) ->
      escaped_defs_exp e1 @ escaped_defs_exp e2 @ escaped_defs_exp e3
  | Apply (Funref _, es, _) -> List.map es ~f:escaped_defs_exp |> List.concat
  | Apply (e, es, _) ->
      escaped_defs_exp e @ (List.map es ~f:escaped_defs_exp |> List.concat)
  | Funref (v, _) -> [v]
  | Lambda (_, _, e) -> escaped_defs_exp e
  | Setbang (_, e) -> escaped_defs_exp e
  | Begin (es, e, _) ->
      (List.map es ~f:escaped_defs_exp |> List.concat) @ escaped_defs_exp e
  | While (e1, e2) -> escaped_defs_exp e1 @ escaped_defs_exp e2

and escaped_defs_prim = function
  | Read -> []
  | Minus e -> escaped_defs_exp e
  | Plus (e1, e2) -> escaped_defs_exp e1 @ escaped_defs_exp e2
  | Subtract (e1, e2) -> escaped_defs_exp e1 @ escaped_defs_exp e2
  | Mult (e1, e2) -> escaped_defs_exp e1 @ escaped_defs_exp e2
  | Div (e1, e2) -> escaped_defs_exp e1 @ escaped_defs_exp e2
  | Rem (e1, e2) -> escaped_defs_exp e1 @ escaped_defs_exp e2
  | Land (e1, e2) -> escaped_defs_exp e1 @ escaped_defs_exp e2
  | Lor (e1, e2) -> escaped_defs_exp e1 @ escaped_defs_exp e2
  | Lxor (e1, e2) -> escaped_defs_exp e1 @ escaped_defs_exp e2
  | Lnot e -> escaped_defs_exp e
  | Eq (e1, e2) -> escaped_defs_exp e1 @ escaped_defs_exp e2
  | Lt (e1, e2) -> escaped_defs_exp e1 @ escaped_defs_exp e2
  | Le (e1, e2) -> escaped_defs_exp e1 @ escaped_defs_exp e2
  | Gt (e1, e2) -> escaped_defs_exp e1 @ escaped_defs_exp e2
  | Ge (e1, e2) -> escaped_defs_exp e1 @ escaped_defs_exp e2
  | Not e -> escaped_defs_exp e
  | And (e1, e2) -> escaped_defs_exp e1 @ escaped_defs_exp e2
  | Or (e1, e2) -> escaped_defs_exp e1 @ escaped_defs_exp e2
  | Vector es -> List.map es ~f:escaped_defs_exp |> List.concat
  | Vectorlength e -> escaped_defs_exp e
  | Vectorref (e, _) -> escaped_defs_exp e
  | Vectorset (e1, _, e2) -> escaped_defs_exp e1 @ escaped_defs_exp e2

let rec recompute_types_def defs = function
  | Def (v, args, t, e) ->
      let e = recompute_types_exp defs (String.Map.of_alist_exn args) e in
      (* this sucks *)
      let t = if String.equal v main then t else typeof_exp e in
      Def (v, args, t, e)

and recompute_types_exp defs env = function
  | Int _ as i -> i
  | Bool _ as b -> b
  | Void -> Void
  | Prim (p, t) ->
      let p, t = recompute_types_prim defs env p in
      Prim (p, t)
  | Var (v, t) -> (
    match Map.find env v with
    | None -> assert false
    | Some t -> Var (v, t) )
  | Let (v, e1, e2, t) ->
      let e1 = recompute_types_exp defs env e1 in
      let t1 = typeof_exp e1 in
      let e2 = recompute_types_exp defs (Map.set env v t1) e2 in
      let t = typeof_exp e2 in
      Let (v, e1, e2, t)
  | If (e1, e2, e3, t) ->
      let e1 = recompute_types_exp defs env e1 in
      let e2 = recompute_types_exp defs env e2 in
      let e3 = recompute_types_exp defs env e3 in
      (* we already passed the type-checker, but
       * `e2` and `e3` may contain different closure
       * environments. for now we just pick one of them.
       * (there should be a better way to do this...) *)
      let t2 = typeof_exp e2 in
      If (e1, e2, e3, t2)
  | Apply (e, es, t) -> (
      let e = recompute_types_exp defs env e in
      let es = List.map es ~f:(recompute_types_exp defs env) in
      match typeof_exp e with
      | Type.Arrow (_, tret) -> Apply (e, es, tret)
      | _ -> Apply (e, es, Type.Trustme) )
  | Funref (v, t) ->
      (* now that the top-level function has the correct type,
       * we need to update the type in the reference to it. *)
      let (Def (_, args, t, _)) =
        List.find_exn defs ~f:(function Def (v', _, _, _) ->
            String.equal v v')
      in
      let t = Type.Arrow (List.map args ~f:snd, t) in
      Funref (v, t)
  | Lambda (args, _, e) ->
      let env =
        List.fold args ~init:env ~f:(fun env (x, t) -> Map.set env x t)
      in
      let e = recompute_types_exp defs env e in
      Lambda (args, typeof_exp e, e)
  | Setbang (v, e) ->
      let e = recompute_types_exp defs env e in
      Setbang (v, e)
  | Begin (es, e, t) ->
      let es = List.map es ~f:(recompute_types_exp defs env) in
      let e = recompute_types_exp defs env e in
      Begin (es, e, typeof_exp e)
  | While (e1, e2) ->
      let e1 = recompute_types_exp defs env e1 in
      let e2 = recompute_types_exp defs env e2 in
      While (e1, e2)

and recompute_types_prim defs env = function
  | Read -> (Read, Type.Integer)
  | Minus e -> (Minus (recompute_types_exp defs env e), Type.Integer)
  | Plus (e1, e2) ->
      ( Plus
          (recompute_types_exp defs env e1, recompute_types_exp defs env e2)
      , Type.Integer )
  | Subtract (e1, e2) ->
      ( Subtract
          (recompute_types_exp defs env e1, recompute_types_exp defs env e2)
      , Type.Integer )
  | Mult (e1, e2) ->
      ( Mult
          (recompute_types_exp defs env e1, recompute_types_exp defs env e2)
      , Type.Integer )
  | Div (e1, e2) ->
      ( Div (recompute_types_exp defs env e1, recompute_types_exp defs env e2)
      , Type.Integer )
  | Rem (e1, e2) ->
      ( Rem (recompute_types_exp defs env e1, recompute_types_exp defs env e2)
      , Type.Integer )
  | Land (e1, e2) ->
      ( Land
          (recompute_types_exp defs env e1, recompute_types_exp defs env e2)
      , Type.Integer )
  | Lor (e1, e2) ->
      ( Lor (recompute_types_exp defs env e1, recompute_types_exp defs env e2)
      , Type.Integer )
  | Lxor (e1, e2) ->
      ( Lxor
          (recompute_types_exp defs env e1, recompute_types_exp defs env e2)
      , Type.Integer )
  | Lnot e -> (Lnot (recompute_types_exp defs env e), Type.Integer)
  | Eq (e1, e2) ->
      ( Eq (recompute_types_exp defs env e1, recompute_types_exp defs env e2)
      , Type.Boolean )
  | Lt (e1, e2) ->
      ( Lt (recompute_types_exp defs env e1, recompute_types_exp defs env e2)
      , Type.Boolean )
  | Le (e1, e2) ->
      ( Le (recompute_types_exp defs env e1, recompute_types_exp defs env e2)
      , Type.Boolean )
  | Gt (e1, e2) ->
      ( Gt (recompute_types_exp defs env e1, recompute_types_exp defs env e2)
      , Type.Boolean )
  | Ge (e1, e2) ->
      ( Ge (recompute_types_exp defs env e1, recompute_types_exp defs env e2)
      , Type.Boolean )
  | Not e -> (Not (recompute_types_exp defs env e), Type.Boolean)
  | And (e1, e2) ->
      ( And (recompute_types_exp defs env e1, recompute_types_exp defs env e2)
      , Type.Boolean )
  | Or (e1, e2) ->
      ( Or (recompute_types_exp defs env e1, recompute_types_exp defs env e2)
      , Type.Boolean )
  | Vector es ->
      let es = List.map es ~f:(recompute_types_exp defs env) in
      let ts = List.map es ~f:typeof_exp in
      (Vector es, Type.Vector ts)
  | Vectorlength e ->
      let e = recompute_types_exp defs env e in
      (Vectorlength e, Type.Integer)
  | Vectorref (e, i) -> (
      let e = recompute_types_exp defs env e in
      match typeof_exp e with
      | Type.Vector ts -> (Vectorref (e, i), List.nth_exn ts i)
      | _ -> (Vectorref (e, i), Type.Trustme) )
  | Vectorset (e1, i, e2) ->
      let e1 = recompute_types_exp defs env e1 in
      let e2 = recompute_types_exp defs env e2 in
      (Vectorset (e1, i, e2), Type.Void)

let rec assigned_and_free_exp e =
  let default () = (String.Set.empty, String.Set.empty) in
  match e with
  | Int _ -> default ()
  | Bool _ -> default ()
  | Void -> default ()
  | Prim (p, _) -> assigned_and_free_prim p
  | Var _ -> default ()
  | Let (_, e1, e2, _) ->
      let a1, f1 = assigned_and_free_exp e1 in
      let a2, f2 = assigned_and_free_exp e2 in
      (Set.union a1 a2, Set.union f1 f2)
  | If (e1, e2, e3, _) ->
      let a1, f1 = assigned_and_free_exp e1 in
      let a2, f2 = assigned_and_free_exp e2 in
      let a3, f3 = assigned_and_free_exp e3 in
      (String.Set.union_list [a1; a2; a3], String.Set.union_list [f1; f2; f3])
  | Apply (e, es, _) ->
      let a, f = assigned_and_free_exp e in
      let as', fs = List.map es ~f:assigned_and_free_exp |> List.unzip in
      (String.Set.union_list (a :: as'), String.Set.union_list (f :: fs))
  | Funref _ -> default ()
  | Lambda (args, _, e) ->
      let fvs =
        let bnd = List.map args ~f:fst |> String.Set.of_list in
        free_vars_of_exp e ~bnd |> Map.to_alist
      in
      let a, f = assigned_and_free_exp e in
      (a, String.Set.(union f (of_list (List.map fvs ~f:fst))))
  | Setbang (v, e) ->
      let a, f = assigned_and_free_exp e in
      (Set.add a v, f)
  | Begin (es, e, _) ->
      let as', fs = List.map es ~f:assigned_and_free_exp |> List.unzip in
      let a, f = assigned_and_free_exp e in
      (String.Set.union_list (a :: as'), String.Set.union_list (f :: fs))
  | While (e1, e2) ->
      let a1, f1 = assigned_and_free_exp e1 in
      let a2, f2 = assigned_and_free_exp e2 in
      (Set.union a1 a2, Set.union f1 f2)

and assigned_and_free_prim p =
  let default () = (String.Set.empty, String.Set.empty) in
  match p with
  | Read -> default ()
  | Minus e -> assigned_and_free_exp e
  | Plus (e1, e2) ->
      let a1, f1 = assigned_and_free_exp e1 in
      let a2, f2 = assigned_and_free_exp e2 in
      (Set.union a1 a2, Set.union f1 f2)
  | Subtract (e1, e2) ->
      let a1, f1 = assigned_and_free_exp e1 in
      let a2, f2 = assigned_and_free_exp e2 in
      (Set.union a1 a2, Set.union f1 f2)
  | Mult (e1, e2) ->
      let a1, f1 = assigned_and_free_exp e1 in
      let a2, f2 = assigned_and_free_exp e2 in
      (Set.union a1 a2, Set.union f1 f2)
  | Div (e1, e2) ->
      let a1, f1 = assigned_and_free_exp e1 in
      let a2, f2 = assigned_and_free_exp e2 in
      (Set.union a1 a2, Set.union f1 f2)
  | Rem (e1, e2) ->
      let a1, f1 = assigned_and_free_exp e1 in
      let a2, f2 = assigned_and_free_exp e2 in
      (Set.union a1 a2, Set.union f1 f2)
  | Land (e1, e2) ->
      let a1, f1 = assigned_and_free_exp e1 in
      let a2, f2 = assigned_and_free_exp e2 in
      (Set.union a1 a2, Set.union f1 f2)
  | Lor (e1, e2) ->
      let a1, f1 = assigned_and_free_exp e1 in
      let a2, f2 = assigned_and_free_exp e2 in
      (Set.union a1 a2, Set.union f1 f2)
  | Lxor (e1, e2) ->
      let a1, f1 = assigned_and_free_exp e1 in
      let a2, f2 = assigned_and_free_exp e2 in
      (Set.union a1 a2, Set.union f1 f2)
  | Lnot e -> assigned_and_free_exp e
  | Eq (e1, e2) ->
      let a1, f1 = assigned_and_free_exp e1 in
      let a2, f2 = assigned_and_free_exp e2 in
      (Set.union a1 a2, Set.union f1 f2)
  | Lt (e1, e2) ->
      let a1, f1 = assigned_and_free_exp e1 in
      let a2, f2 = assigned_and_free_exp e2 in
      (Set.union a1 a2, Set.union f1 f2)
  | Le (e1, e2) ->
      let a1, f1 = assigned_and_free_exp e1 in
      let a2, f2 = assigned_and_free_exp e2 in
      (Set.union a1 a2, Set.union f1 f2)
  | Gt (e1, e2) ->
      let a1, f1 = assigned_and_free_exp e1 in
      let a2, f2 = assigned_and_free_exp e2 in
      (Set.union a1 a2, Set.union f1 f2)
  | Ge (e1, e2) ->
      let a1, f1 = assigned_and_free_exp e1 in
      let a2, f2 = assigned_and_free_exp e2 in
      (Set.union a1 a2, Set.union f1 f2)
  | Not e -> assigned_and_free_exp e
  | And (e1, e2) ->
      let a1, f1 = assigned_and_free_exp e1 in
      let a2, f2 = assigned_and_free_exp e2 in
      (Set.union a1 a2, Set.union f1 f2)
  | Or (e1, e2) ->
      let a1, f1 = assigned_and_free_exp e1 in
      let a2, f2 = assigned_and_free_exp e2 in
      (Set.union a1 a2, Set.union f1 f2)
  | Vector es ->
      let as', fs = List.map es ~f:assigned_and_free_exp |> List.unzip in
      (String.Set.union_list as', String.Set.union_list fs)
  | Vectorlength e -> assigned_and_free_exp e
  | Vectorref (e, _) -> assigned_and_free_exp e
  | Vectorset (e1, _, e2) ->
      let a1, f1 = assigned_and_free_exp e1 in
      let a2, f2 = assigned_and_free_exp e2 in
      (Set.union a1 a2, Set.union f1 f2)

let rec convert_assignments = function
  | Program (info, defs) ->
      let defs = List.map defs ~f:convert_assignments_def in
      (* should try to reach a fixed-point *)
      let defs = List.map defs ~f:(recompute_types_def defs) in
      Program (info, defs)

and convert_assignments_def = function
  | Def (v, args, typ, e) ->
      let a, f = assigned_and_free_exp e in
      let args', renamed_args =
        let inter = Set.inter a f in
        List.fold args ~init:([], []) ~f:(fun (args, renamed_args) (x, t) ->
            if Set.mem inter x then
              (("_param_" ^ x, t) :: args, (x, t) :: renamed_args)
            else ((x, t) :: args, renamed_args))
      in
      let e = convert_assignments_exp a f e in
      let renamed_args = List.rev renamed_args in
      let e =
        List.fold_right renamed_args ~init:e ~f:(fun (x, t) e ->
            Let
              ( x
              , Prim (Vector [Var ("_param_" ^ x, t)], Type.Vector [t])
              , e
              , typ ))
      in
      Def (v, List.rev args', typ, e)

and convert_assignments_exp a f = function
  | Int _ as i -> i
  | Bool _ as b -> b
  | Void -> Void
  | Prim (p, t) -> Prim (convert_assignments_prim a f p, t)
  | Var (v, t) as var ->
      if Set.mem a v && Set.mem f v then
        Prim (Vectorref (Var (v, Type.Vector [t]), 0), t)
      else var
  | Let (v, e1, e2, t) ->
      let e1 = convert_assignments_exp a f e1 in
      let e2 = convert_assignments_exp a f e2 in
      if Set.mem a v && Set.mem f v then
        Let (v, Prim (Vector [e1], Type.Vector [typeof_exp e1]), e2, t)
      else Let (v, e1, e2, t)
  | If (e1, e2, e3, t) ->
      let e1 = convert_assignments_exp a f e1 in
      let e2 = convert_assignments_exp a f e2 in
      let e3 = convert_assignments_exp a f e3 in
      If (e1, e2, e3, t)
  | Apply (e, es, t) ->
      let e = convert_assignments_exp a f e in
      let es = List.map es ~f:(convert_assignments_exp a f) in
      Apply (e, es, t)
  | Funref _ as f -> f
  | Lambda (args, typ, e) ->
      let args', renamed_args =
        let inter = Set.inter a f in
        List.fold args ~init:([], []) ~f:(fun (args, renamed_args) (x, t) ->
            if Set.mem inter x then
              (("_param_" ^ x, t) :: args, (x, t) :: renamed_args)
            else ((x, t) :: args, renamed_args))
      in
      let e = convert_assignments_exp a f e in
      let renamed_args = List.rev renamed_args in
      let e =
        List.fold_right renamed_args ~init:e ~f:(fun (x, t) e ->
            Let
              ( x
              , Prim (Vector [Var ("_param_" ^ x, t)], Type.Vector [t])
              , e
              , typ ))
      in
      Lambda (List.rev args', typ, e)
  | Setbang (v, e) as s ->
      if Set.mem a v && Set.mem f v then
        Prim
          ( Vectorset
              ( Var (v, Type.Vector [typeof_exp e])
              , 0
              , convert_assignments_exp a f e )
          , Type.Void )
      else s
  | Begin (es, e, t) ->
      let es = List.map es ~f:(convert_assignments_exp a f) in
      let e = convert_assignments_exp a f e in
      Begin (es, e, t)
  | While (e1, e2) ->
      let e1 = convert_assignments_exp a f e1 in
      let e2 = convert_assignments_exp a f e2 in
      While (e1, e2)

and convert_assignments_prim a f = function
  | Read -> Read
  | Minus e -> Minus (convert_assignments_exp a f e)
  | Plus (e1, e2) ->
      Plus (convert_assignments_exp a f e1, convert_assignments_exp a f e2)
  | Subtract (e1, e2) ->
      Subtract
        (convert_assignments_exp a f e1, convert_assignments_exp a f e2)
  | Mult (e1, e2) ->
      Mult (convert_assignments_exp a f e1, convert_assignments_exp a f e2)
  | Div (e1, e2) ->
      Div (convert_assignments_exp a f e1, convert_assignments_exp a f e2)
  | Rem (e1, e2) ->
      Rem (convert_assignments_exp a f e1, convert_assignments_exp a f e2)
  | Land (e1, e2) ->
      Land (convert_assignments_exp a f e1, convert_assignments_exp a f e2)
  | Lor (e1, e2) ->
      Lor (convert_assignments_exp a f e1, convert_assignments_exp a f e2)
  | Lxor (e1, e2) ->
      Lxor (convert_assignments_exp a f e1, convert_assignments_exp a f e2)
  | Lnot e -> Lnot (convert_assignments_exp a f e)
  | Eq (e1, e2) ->
      Eq (convert_assignments_exp a f e1, convert_assignments_exp a f e2)
  | Lt (e1, e2) ->
      Lt (convert_assignments_exp a f e1, convert_assignments_exp a f e2)
  | Le (e1, e2) ->
      Le (convert_assignments_exp a f e1, convert_assignments_exp a f e2)
  | Gt (e1, e2) ->
      Gt (convert_assignments_exp a f e1, convert_assignments_exp a f e2)
  | Ge (e1, e2) ->
      Ge (convert_assignments_exp a f e1, convert_assignments_exp a f e2)
  | Not e -> Not (convert_assignments_exp a f e)
  | And (e1, e2) ->
      And (convert_assignments_exp a f e1, convert_assignments_exp a f e2)
  | Or (e1, e2) ->
      Or (convert_assignments_exp a f e1, convert_assignments_exp a f e2)
  | Vector es -> Vector (List.map es ~f:(convert_assignments_exp a f))
  | Vectorlength e -> Vectorlength (convert_assignments_exp a f e)
  | Vectorref (e, i) -> Vectorref (convert_assignments_exp a f e, i)
  | Vectorset (e1, i, e2) ->
      Vectorset
        (convert_assignments_exp a f e1, i, convert_assignments_exp a f e2)

let rec needs_closures = function
  | Program (_, defs) -> List.exists defs ~f:needs_closures_def

and needs_closures_def = function
  | Def (_, _, _, e) -> needs_closures_exp e

and needs_closures_exp = function
  | Int _ -> false
  | Bool _ -> false
  | Void -> false
  | Prim (p, _) -> needs_closures_prim p
  | Var _ -> false
  | Let (_, e1, e2, _) -> needs_closures_exp e1 || needs_closures_exp e2
  | If (e1, e2, e3, _) ->
      needs_closures_exp e1 || needs_closures_exp e2 || needs_closures_exp e3
  | Apply (e, es, _) ->
      needs_closures_exp e || List.exists es ~f:needs_closures_exp
  | Funref _ -> false
  | Lambda _ -> true
  | Setbang (_, e) -> needs_closures_exp e
  | Begin (es, e, _) ->
      List.exists es ~f:needs_closures_exp || needs_closures_exp e
  | While (e1, e2) -> needs_closures_exp e1 || needs_closures_exp e2

and needs_closures_prim = function
  | Read -> false
  | Minus e -> needs_closures_exp e
  | Plus (e1, e2) -> needs_closures_exp e1 || needs_closures_exp e2
  | Subtract (e1, e2) -> needs_closures_exp e1 || needs_closures_exp e2
  | Mult (e1, e2) -> needs_closures_exp e1 || needs_closures_exp e2
  | Div (e1, e2) -> needs_closures_exp e1 || needs_closures_exp e2
  | Rem (e1, e2) -> needs_closures_exp e1 || needs_closures_exp e2
  | Land (e1, e2) -> needs_closures_exp e1 || needs_closures_exp e2
  | Lor (e1, e2) -> needs_closures_exp e1 || needs_closures_exp e2
  | Lxor (e1, e2) -> needs_closures_exp e1 || needs_closures_exp e2
  | Lnot e -> needs_closures_exp e
  | Eq (e1, e2) -> needs_closures_exp e1 || needs_closures_exp e2
  | Lt (e1, e2) -> needs_closures_exp e1 || needs_closures_exp e2
  | Le (e1, e2) -> needs_closures_exp e1 || needs_closures_exp e2
  | Gt (e1, e2) -> needs_closures_exp e1 || needs_closures_exp e2
  | Ge (e1, e2) -> needs_closures_exp e1 || needs_closures_exp e2
  | Not e -> needs_closures_exp e
  | And (e1, e2) -> needs_closures_exp e1 || needs_closures_exp e2
  | Or (e1, e2) -> needs_closures_exp e1 || needs_closures_exp e2
  | Vector es -> List.exists es ~f:needs_closures_exp
  | Vectorlength e -> needs_closures_exp e
  | Vectorref (e, _) -> needs_closures_exp e
  | Vectorset (e1, _, e2) -> needs_closures_exp e1 || needs_closures_exp e2

let rec convert_to_closures = function
  | Program (info, defs) as p ->
      if not (needs_closures p) then p
      else
        let escaped = escaped_defs p in
        let n = ref 0 in
        let defs, new_defs =
          List.(map defs ~f:(convert_to_closures_def escaped n) |> unzip)
        in
        let defs = List.concat new_defs @ defs in
        Program (info, List.map defs ~f:(recompute_types_def defs))

and convert_to_closures_def escaped n = function
  | Def (v, args, t, e) ->
      let is_escaped = Set.mem escaped v in
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
        convert_to_closures_exp escaped (String.Map.of_alist_exn args) n e
      in
      let args =
        if (not is_escaped) || Label.equal v main then args
        else ("_", Type.Vector []) :: args
      in
      let t = typeof_exp e' in
      (Def (v, args, t, e'), new_defs)

(* IMPORTANT: the types in the AST will be partly junk
 * after running this, so it's our job to fix them *)
and convert_to_closures_exp escaped env n = function
  | Int _ as i -> (i, [])
  | Bool _ as b -> (b, [])
  | Void -> (Void, [])
  | Prim (p, t) ->
      let p, new_defs = convert_to_closures_prim escaped env n p in
      (Prim (p, t), new_defs)
  | Var (v, t) ->
      let t =
        match Map.find env v with
        | None -> t
        | Some t -> t
      in
      (Var (v, t), [])
  | Let (v, e1, e2, t) ->
      let e1, new_defs1 = convert_to_closures_exp escaped env n e1 in
      let e2, new_defs2 =
        convert_to_closures_exp escaped (Map.set env v (typeof_exp e1)) n e2
      in
      let t = typeof_exp e2 in
      (Let (v, e1, e2, t), new_defs1 @ new_defs2)
  | If (e1, e2, e3, t) ->
      let e1, new_defs1 = convert_to_closures_exp escaped env n e1 in
      let e2, new_defs2 = convert_to_closures_exp escaped env n e2 in
      let e3, new_defs3 = convert_to_closures_exp escaped env n e3 in
      (If (e1, e2, e3, t), new_defs1 @ new_defs2 @ new_defs3)
  | Apply ((Funref (v, _) as f), es, t) when not (Set.mem escaped v) ->
      let es, new_defs_es =
        List.(map es ~f:(convert_to_closures_exp escaped env n) |> unzip)
      in
      (Apply (f, es, t), List.concat new_defs_es)
  | Apply (e, es, t) ->
      let e, new_defs = convert_to_closures_exp escaped env n e in
      let es, new_defs_es =
        List.(map es ~f:(convert_to_closures_exp escaped env n) |> unzip)
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
  | Funref (v, t) as f ->
      if Set.mem escaped v then
        ( Prim
            ( Vector [Funref (v, t); Prim (Vector [], Type.Vector [])]
            , Type.(Vector [t; Vector []]) )
        , [] )
      else (f, [])
  | Lambda (args, t, e) ->
      let free_vars =
        let bnd = List.map args ~f:fst |> String.Set.of_list in
        free_vars_of_exp e ~bnd |> Map.to_alist
      in
      let d = newclo n in
      let ct = Type.Vector (List.map free_vars ~f:snd) in
      let clo_arg = ("_c", ct) in
      let e, new_defs =
        let env =
          List.fold args ~init:env ~f:(fun env (x, t) -> Map.set env x t)
        in
        convert_to_closures_exp escaped env n e
      in
      let et = typeof_exp e in
      let new_body, _ =
        List.fold_right free_vars
          ~init:(e, List.length free_vars - 1)
          ~f:(fun (x, t) (e, i) ->
            (Let (x, Prim (Vectorref (Var ("_c", ct), i), t), e, et), i - 1))
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
  | Setbang (v, e) ->
      let e, new_defs = convert_to_closures_exp escaped env n e in
      (Setbang (v, e), new_defs)
  | Begin (es, e, t) ->
      let es, new_defs_es =
        List.map es ~f:(convert_to_closures_exp escaped env n) |> List.unzip
      in
      let e, new_defs = convert_to_closures_exp escaped env n e in
      (Begin (es, e, typeof_exp e), List.concat new_defs_es @ new_defs)
  | While (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp escaped env n e1 in
      let e2, new_defs2 = convert_to_closures_exp escaped env n e2 in
      (While (e1, e2), new_defs1 @ new_defs2)

and convert_to_closures_prim escaped env n = function
  | Read -> (Read, [])
  | Minus e ->
      let e, new_defs = convert_to_closures_exp escaped env n e in
      (Minus e, new_defs)
  | Plus (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp escaped env n e1 in
      let e2, new_defs2 = convert_to_closures_exp escaped env n e2 in
      (Plus (e1, e2), new_defs1 @ new_defs2)
  | Subtract (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp escaped env n e1 in
      let e2, new_defs2 = convert_to_closures_exp escaped env n e2 in
      (Subtract (e1, e2), new_defs1 @ new_defs2)
  | Mult (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp escaped env n e1 in
      let e2, new_defs2 = convert_to_closures_exp escaped env n e2 in
      (Mult (e1, e2), new_defs1 @ new_defs2)
  | Div (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp escaped env n e1 in
      let e2, new_defs2 = convert_to_closures_exp escaped env n e2 in
      (Div (e1, e2), new_defs1 @ new_defs2)
  | Rem (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp escaped env n e1 in
      let e2, new_defs2 = convert_to_closures_exp escaped env n e2 in
      (Rem (e1, e2), new_defs1 @ new_defs2)
  | Land (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp escaped env n e1 in
      let e2, new_defs2 = convert_to_closures_exp escaped env n e2 in
      (Land (e1, e2), new_defs1 @ new_defs2)
  | Lor (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp escaped env n e1 in
      let e2, new_defs2 = convert_to_closures_exp escaped env n e2 in
      (Lor (e1, e2), new_defs1 @ new_defs2)
  | Lxor (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp escaped env n e1 in
      let e2, new_defs2 = convert_to_closures_exp escaped env n e2 in
      (Lxor (e1, e2), new_defs1 @ new_defs2)
  | Lnot e ->
      let e, new_defs = convert_to_closures_exp escaped env n e in
      (Lnot e, new_defs)
  | Eq (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp escaped env n e1 in
      let e2, new_defs2 = convert_to_closures_exp escaped env n e2 in
      (Eq (e1, e2), new_defs1 @ new_defs2)
  | Lt (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp escaped env n e1 in
      let e2, new_defs2 = convert_to_closures_exp escaped env n e2 in
      (Lt (e1, e2), new_defs1 @ new_defs2)
  | Le (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp escaped env n e1 in
      let e2, new_defs2 = convert_to_closures_exp escaped env n e2 in
      (Le (e1, e2), new_defs1 @ new_defs2)
  | Gt (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp escaped env n e1 in
      let e2, new_defs2 = convert_to_closures_exp escaped env n e2 in
      (Gt (e1, e2), new_defs1 @ new_defs2)
  | Ge (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp escaped env n e1 in
      let e2, new_defs2 = convert_to_closures_exp escaped env n e2 in
      (Ge (e1, e2), new_defs1 @ new_defs2)
  | Not e ->
      let e, new_defs = convert_to_closures_exp escaped env n e in
      (Not e, new_defs)
  | And (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp escaped env n e1 in
      let e2, new_defs2 = convert_to_closures_exp escaped env n e2 in
      (And (e1, e2), new_defs1 @ new_defs2)
  | Or (e1, e2) ->
      let e1, new_defs1 = convert_to_closures_exp escaped env n e1 in
      let e2, new_defs2 = convert_to_closures_exp escaped env n e2 in
      (Or (e1, e2), new_defs1 @ new_defs2)
  | Vector es ->
      let es, new_defs =
        List.(map es ~f:(convert_to_closures_exp escaped env n) |> unzip)
      in
      (Vector es, List.concat new_defs)
  | Vectorlength e ->
      let e, new_defs = convert_to_closures_exp escaped env n e in
      (Vectorlength e, new_defs)
  | Vectorref (e, i) ->
      let e, new_defs = convert_to_closures_exp escaped env n e in
      (Vectorref (e, i), new_defs)
  | Vectorset (e1, i, e2) ->
      let e1, new_defs1 = convert_to_closures_exp escaped env n e1 in
      let e2, new_defs2 = convert_to_closures_exp escaped env n e2 in
      (Vectorset (e1, i, e2), new_defs1 @ new_defs2)

and newclo n =
  let d = Printf.sprintf "_clo%d" !n in
  incr n; d

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
  | Setbang (v, e) -> Setbang (v, limit_functions_exp defs e)
  | Begin (es, e, t) ->
      Begin
        ( List.map es ~f:(limit_functions_exp defs)
        , limit_functions_exp defs e
        , t )
  | While (e1, e2) ->
      While (limit_functions_exp defs e1, limit_functions_exp defs e2)

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

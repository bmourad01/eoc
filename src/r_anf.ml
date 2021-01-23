open Core_kernel

type var = R_alloc.var

module Type = R_alloc.Type
module Type_map = R_alloc.Type_map

type type_env = R_alloc.type_env

type info = unit

type t = Program of info * def list

and def = Def of var * (var * Type.t) list * Type.t * exp

and exp =
  | Atom of atom
  | Prim of prim * Type.t
  | Let of var * exp * exp * Type.t
  | If of exp * exp * exp * Type.t
  | Apply of atom * atom list * Type.t
  | Funref of var * Type.t
  | Collect of int
  | Allocate of int * Type.t
  | Globalvalue of string * Type.t

and atom = Int of Int64.t | Bool of bool | Var of var * Type.t | Void

and prim =
  | Read
  | Minus of atom
  | Plus of atom * atom
  | Subtract of atom * atom
  | Mult of atom * atom
  | Div of atom * atom
  | Rem of atom * atom
  | Land of atom * atom
  | Lor of atom * atom
  | Lxor of atom * atom
  | Lnot of atom
  | Eq of atom * atom
  | Lt of atom * atom
  | Le of atom * atom
  | Gt of atom * atom
  | Ge of atom * atom
  | Not of atom
  | Vectorlength of atom
  | Vectorref of atom * int
  | Vectorset of atom * int * atom

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
  | Atom a -> string_of_atom a
  | Prim (p, _) -> string_of_prim p
  | Let (v, e1, e2, _) ->
      Printf.sprintf "(let ([%s %s]) %s)" v (string_of_exp e1)
        (string_of_exp e2)
  | If (e1, e2, e3, _) ->
      Printf.sprintf "(if %s %s %s)" (string_of_exp e1) (string_of_exp e2)
        (string_of_exp e3)
  | Apply (a, [], _) -> Printf.sprintf "(%s)" (string_of_atom a)
  | Apply (a, as', _) ->
      Printf.sprintf "(%s %s)" (string_of_atom a)
        (List.map as' ~f:string_of_atom |> String.concat ~sep:" ")
  | Funref (v, _) -> Printf.sprintf "(fun-ref %s)" v
  | Collect n -> Printf.sprintf "(collect %d)" n
  | Allocate (n, t) -> Printf.sprintf "(allocate %d %s)" n (Type.to_string t)
  | Globalvalue (v, _) -> Printf.sprintf "(global-value '%s)" v

and string_of_atom = function
  | Int i -> Int64.to_string i
  | Bool b -> if b then "#t" else "#f"
  | Var (v, _) -> v
  | Void -> "(void)"

and string_of_prim = function
  | Read -> "(read)"
  | Minus a -> Printf.sprintf "(- %s)" (string_of_atom a)
  | Plus (a1, a2) ->
      Printf.sprintf "(+ %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Subtract (a1, a2) ->
      Printf.sprintf "(- %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Mult (a1, a2) ->
      Printf.sprintf "(* %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Div (a1, a2) ->
      Printf.sprintf "(/ %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Rem (a1, a2) ->
      Printf.sprintf "(rem %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Land (a1, a2) ->
      Printf.sprintf "(land %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Lor (a1, a2) ->
      Printf.sprintf "(lor %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Lxor (a1, a2) ->
      Printf.sprintf "(lxor %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Lnot a -> Printf.sprintf "(lnot %s)" (string_of_atom a)
  | Eq (a1, a2) ->
      Printf.sprintf "(eq? %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Lt (a1, a2) ->
      Printf.sprintf "(< %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Le (a1, a2) ->
      Printf.sprintf "(<= %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Gt (a1, a2) ->
      Printf.sprintf "(> %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Ge (a1, a2) ->
      Printf.sprintf "(>= %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Not a -> Printf.sprintf "(not %s)" (string_of_atom a)
  | Vectorlength a -> Printf.sprintf "(vector-length %s)" (string_of_atom a)
  | Vectorref (a, i) ->
      Printf.sprintf "(vector-ref %s %d)" (string_of_atom a) i
  | Vectorset (a1, i, a2) ->
      Printf.sprintf "(vector-set! %s %d %s)" (string_of_atom a1) i
        (string_of_atom a2)

let rec resolve_complex = function
  | R_alloc.Program (info, defs) ->
      Program ((), List.map defs ~f:(resolve_complex_def info.nvars))

and resolve_complex_def nvars = function
  | R_alloc.Def (v, args, t, e) ->
      let n = ref (Map.find_exn nvars v) in
      let m =
        List.map args ~f:(fun (x, t) ->
            let x' = fresh_var n in
            (x, Var (x', t)))
        |> String.Map.of_alist_exn
      in
      let args =
        List.map args ~f:(fun (x, t) ->
            match Map.find_exn m x with
            | Var (x', _) -> (x', t)
            | _ -> assert false)
      in
      let e = resolve_complex_exp m n e in
      Def (v, args, t, e)

and resolve_complex_atom m n = function
  | R_alloc.Int i -> ([], Int i)
  | R_alloc.Bool b -> ([], Bool b)
  | R_alloc.Void -> ([], Void)
  | R_alloc.(Prim (Read, t)) ->
      let x = fresh_var n in
      ([(x, Prim (Read, t))], Var (x, t))
  | R_alloc.(Prim (Minus e, t)) ->
      let nv, a = resolve_complex_atom m n e in
      let x = fresh_var n in
      (nv @ [(x, Prim (Minus a, t))], Var (x, t))
  | R_alloc.(Prim (Plus (e1, Prim (Minus e2, _)), t)) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Subtract (a1, a2), t))], Var (x, t))
  | R_alloc.(Prim (Plus (Prim (Minus e1, _), e2), t)) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Subtract (a2, a1), t))], Var (x, t))
  | R_alloc.(Prim (Plus (e1, e2), t)) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Plus (a1, a2), t))], Var (x, t))
  | R_alloc.(Prim (Subtract (e1, e2), t)) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Subtract (a1, a2), t))], Var (x, t))
  | R_alloc.(Prim (Mult (e, Int -1L), t))
   |R_alloc.(Prim (Mult (Int -1L, e), t)) ->
      let nv, a = resolve_complex_atom m n e in
      let x = fresh_var n in
      (nv @ [(x, Prim (Minus a, t))], Var (x, t))
  | R_alloc.(Prim (Mult (e1, e2), t)) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Mult (a1, a2), t))], Var (x, t))
  | R_alloc.(Prim (Div (e1, e2), t)) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Div (a1, a2), t))], Var (x, t))
  | R_alloc.(Prim (Rem (e1, e2), t)) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Rem (a1, a2), t))], Var (x, t))
  | R_alloc.(Prim (Land (e1, e2), t)) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Land (a1, a2), t))], Var (x, t))
  | R_alloc.(Prim (Lor (e1, e2), t)) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Lor (a1, a2), t))], Var (x, t))
  | R_alloc.(Prim (Lxor (e1, e2), t)) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Lxor (a1, a2), t))], Var (x, t))
  | R_alloc.(Prim (Lnot e, t)) ->
      let nv, a = resolve_complex_atom m n e in
      let x = fresh_var n in
      (nv @ [(x, Prim (Lnot a, t))], Var (x, t))
  | R_alloc.(Prim (Eq (e1, e2), t)) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Eq (a1, a2), t))], Var (x, t))
  | R_alloc.(Prim (Lt (e1, e2), t)) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Lt (a1, a2), t))], Var (x, t))
  | R_alloc.(Prim (Le (e1, e2), t)) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Le (a1, a2), t))], Var (x, t))
  | R_alloc.(Prim (Gt (e1, e2), t)) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Gt (a1, a2), t))], Var (x, t))
  | R_alloc.(Prim (Ge (e1, e2), t)) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Ge (a1, a2), t))], Var (x, t))
  | R_alloc.(Prim (Not e, t)) ->
      let nv, a = resolve_complex_atom m n e in
      let x = fresh_var n in
      (nv @ [(x, Prim (Not a, t))], Var (x, t))
  | R_alloc.(Prim (And (e1, e2), t)) ->
      let e =
        If
          ( resolve_complex_exp m n e1
          , resolve_complex_exp m n e2
          , Atom (Bool false)
          , t )
      in
      let x = fresh_var n in
      ([(x, e)], Var (x, t))
  | R_alloc.(Prim (Or (e1, e2), t)) ->
      let e =
        If
          ( resolve_complex_exp m n e1
          , Atom (Bool true)
          , resolve_complex_exp m n e2
          , t )
      in
      let x = fresh_var n in
      ([(x, e)], Var (x, t))
  | R_alloc.(Prim (Vectorlength e, t)) ->
      let nv, a = resolve_complex_atom m n e in
      let x = fresh_var n in
      (nv @ [(x, Prim (Vectorlength a, t))], Var (x, t))
  | R_alloc.(Prim (Vectorref (e, i), t)) ->
      let nv, a = resolve_complex_atom m n e in
      let x = fresh_var n in
      (nv @ [(x, Prim (Vectorref (a, i), t))], Var (x, t))
  | R_alloc.(Prim (Vectorset (e1, i, e2), t)) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Vectorset (a1, i, a2), t))], Var (x, t))
  | R_alloc.Var (v, _) -> (
    match Map.find m v with
    | None ->
        failwith ("R_anf.resolve_complex_atom: var " ^ v ^ " is unbound")
    | Some e -> ([], e) )
  | R_alloc.Let (v, e1, e2, _) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom (Map.set m v a1) n e2 in
      (nv1 @ nv2, a2)
  | R_alloc.If (e1, e2, e3, t) ->
      let e =
        If
          ( resolve_complex_exp m n e1
          , resolve_complex_exp m n e2
          , resolve_complex_exp m n e3
          , t )
      in
      let x = fresh_var n in
      ([(x, e)], Var (x, t))
  | R_alloc.Apply (e, es, t) ->
      let nv, a = resolve_complex_atom m n e in
      let nvs, as' =
        List.map es ~f:(resolve_complex_atom m n) |> List.unzip
      in
      let nvs = List.concat nvs in
      let x = fresh_var n in
      ((nv @ nvs) @ [(x, Apply (a, as', t))], Var (x, t))
  | R_alloc.Funref (v, t) ->
      let e = Funref (v, t) in
      let x = fresh_var n in
      ([(x, e)], Var (x, t))
  | R_alloc.Collect n' ->
      let e = Collect n' in
      let x = fresh_var n in
      ([(x, e)], Var (x, Type.Void))
  | R_alloc.Allocate (n', t) ->
      let e = Allocate (n', t) in
      let x = fresh_var n in
      ([(x, e)], Var (x, t))
  | R_alloc.Globalvalue (v, t) ->
      let e = Globalvalue (v, t) in
      let x = fresh_var n in
      ([(x, e)], Var (x, t))

and resolve_complex_exp m n = function
  | R_alloc.Int i -> Atom (Int i)
  | R_alloc.Bool b -> Atom (Bool b)
  | R_alloc.Void -> Atom Void
  | R_alloc.Prim (Read, t) -> Prim (Read, t)
  | R_alloc.Prim (Minus e, t) ->
      let nv, a = resolve_complex_atom m n e in
      unfold nv (Prim (Minus a, t))
  | R_alloc.Prim (Plus (e1, e2), t) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      unfold (nv1 @ nv2) (Prim (Plus (a1, a2), t))
  | R_alloc.Prim (Subtract (e1, e2), t) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      unfold (nv1 @ nv2) (Prim (Subtract (a1, a2), t))
  | R_alloc.Prim (Mult (e1, e2), t) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      unfold (nv1 @ nv2) (Prim (Mult (a1, a2), t))
  | R_alloc.Prim (Div (e1, e2), t) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      unfold (nv1 @ nv2) (Prim (Div (a1, a2), t))
  | R_alloc.Prim (Rem (e1, e2), t) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      unfold (nv1 @ nv2) (Prim (Rem (a1, a2), t))
  | R_alloc.Prim (Land (e1, e2), t) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      unfold (nv1 @ nv2) (Prim (Land (a1, a2), t))
  | R_alloc.Prim (Lor (e1, e2), t) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      unfold (nv1 @ nv2) (Prim (Lor (a1, a2), t))
  | R_alloc.Prim (Lxor (e1, e2), t) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      unfold (nv1 @ nv2) (Prim (Lxor (a1, a2), t))
  | R_alloc.Prim (Lnot e, t) ->
      let nv, a = resolve_complex_atom m n e in
      unfold nv (Prim (Lnot a, t))
  | R_alloc.Prim (Eq (e1, e2), t) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      unfold (nv1 @ nv2) (Prim (Eq (a1, a2), t))
  | R_alloc.Prim (Lt (e1, e2), t) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      unfold (nv1 @ nv2) (Prim (Lt (a1, a2), t))
  | R_alloc.Prim (Le (e1, e2), t) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      unfold (nv1 @ nv2) (Prim (Le (a1, a2), t))
  | R_alloc.Prim (Gt (e1, e2), t) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      unfold (nv1 @ nv2) (Prim (Gt (a1, a2), t))
  | R_alloc.Prim (Ge (e1, e2), t) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      unfold (nv1 @ nv2) (Prim (Ge (a1, a2), t))
  | R_alloc.Prim (Not e, t) ->
      let nv, a = resolve_complex_atom m n e in
      unfold nv (Prim (Not a, t))
  | R_alloc.Prim (And (e1, e2), t) ->
      If
        ( resolve_complex_exp m n e1
        , resolve_complex_exp m n e2
        , Atom (Bool false)
        , t )
  | R_alloc.Prim (Or (e1, e2), t) ->
      If
        ( resolve_complex_exp m n e1
        , Atom (Bool true)
        , resolve_complex_exp m n e2
        , t )
  | R_alloc.Prim (Vectorlength e, t) ->
      let nv, a = resolve_complex_atom m n e in
      unfold nv (Prim (Vectorlength a, t))
  | R_alloc.Prim (Vectorref (e, i), t) ->
      (* we still have to expand this expression anyway
       * since it can be the predicate of an if expression *)
      let nv, a = resolve_complex_atom m n e in
      let x = fresh_var n in
      unfold (nv @ [(x, Prim (Vectorref (a, i), t))]) (Atom (Var (x, t)))
  | R_alloc.Prim (Vectorset (e1, i, e2), t) ->
      let nv1, a1 = resolve_complex_atom m n e1 in
      let nv2, a2 = resolve_complex_atom m n e2 in
      unfold (nv1 @ nv2) (Prim (Vectorset (a1, i, a2), t))
  | R_alloc.Var (v, _) -> (
    match Map.find m v with
    | None -> failwith ("R_anf.resolve_complex_exp: var " ^ v ^ " is unbound")
    | Some e -> Atom e )
  | R_alloc.Let (v, e1, e2, t) ->
      let x = fresh_var n in
      Let
        ( x
        , resolve_complex_exp m n e1
        , resolve_complex_exp (Map.set m v (Var (x, typeof' e1))) n e2
        , t )
  | R_alloc.If (e1, e2, e3, t) ->
      If
        ( resolve_complex_exp m n e1
        , resolve_complex_exp m n e2
        , resolve_complex_exp m n e3
        , t )
  | R_alloc.Apply (e, es, t) ->
      (* same situation as vector-ref *)
      let nv, a = resolve_complex_atom m n e in
      let nvs, as' =
        List.map es ~f:(resolve_complex_atom m n) |> List.unzip
      in
      let nvs = List.concat nvs in
      let x = fresh_var n in
      unfold (nv @ nvs @ [(x, Apply (a, as', t))]) (Atom (Var (x, t)))
  | R_alloc.Funref (v, t) -> Funref (v, t)
  | R_alloc.Collect n -> Collect n
  | R_alloc.Allocate (n, t) -> Allocate (n, t)
  | R_alloc.Globalvalue (v, t) -> Globalvalue (v, t)

and fresh_var n =
  let x = Printf.sprintf "%%%d" !n in
  incr n; x

and unfold nv e =
  List.fold_right nv ~init:e ~f:(fun (v, e) acc ->
      Let (v, e, acc, typeof acc))

and typeof' = function
  | R_alloc.Int _ -> Type.Integer
  | R_alloc.Bool _ -> Type.Boolean
  | R_alloc.Void -> Type.Void
  | R_alloc.Prim (_, t) -> t
  | R_alloc.Var (_, t) -> t
  | R_alloc.Let (_, _, _, t) -> t
  | R_alloc.If (_, _, _, t) -> t
  | R_alloc.Apply (_, _, t) -> t
  | R_alloc.Funref (_, t) -> t
  | R_alloc.Collect _ -> Type.Void
  | R_alloc.Allocate (_, t) -> t
  | R_alloc.Globalvalue (_, t) -> t

and typeof = function
  | Atom (Int _) -> Type.Integer
  | Atom (Bool _) -> Type.Boolean
  | Atom (Var (_, t)) -> t
  | Atom Void -> Type.Void
  | Prim (_, t) -> t
  | Let (_, _, _, t) -> t
  | If (_, _, _, t) -> t
  | Apply (_, _, t) -> t
  | Funref (_, t) -> t
  | Collect _ -> Type.Void
  | Allocate (_, t) -> t
  | Globalvalue (_, t) -> t

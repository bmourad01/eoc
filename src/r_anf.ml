open Core_kernel

type info = R.info

type var = R.var

type t = Program of info * exp

and exp =
  | Atom of atom
  | Prim of prim
  | Let of var * exp * exp
  | If of exp * exp * exp

and atom = Int of int | Bool of bool | Var of var

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

let rec to_string = function
  | Program (_, e) -> string_of_exp e

and string_of_exp = function
  | Atom a -> string_of_atom a
  | Prim p -> string_of_prim p
  | Let (v, e1, e2) ->
      Printf.sprintf "(let ([%s %s]) %s)" v (string_of_exp e1)
        (string_of_exp e2)
  | If (e1, e2, e3) ->
      Printf.sprintf "(if %s %s %s)" (string_of_exp e1) (string_of_exp e2)
        (string_of_exp e3)

and string_of_atom = function
  | Int i -> Int.to_string i
  | Bool b -> if b then "#t" else "#f"
  | Var v -> v

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

let rec resolve_complex = function
  | R.Program (info, e) ->
      let nv, a = resolve_complex_exp R.empty_var_env (ref 1) e in
      Program (info, unfold nv (Atom a))

and resolve_complex_exp m n = function
  | R.Int i -> ([], Int i)
  | R.Bool b -> ([], Bool b)
  | R.(Prim Read) ->
      let x = fresh_var n in
      ([(x, Prim Read)], Var x)
  | R.(Prim (Minus e)) ->
      let nv, a = resolve_complex_exp m n e in
      let x = fresh_var n in
      (nv @ [(x, Prim (Minus a))], Var x)
  | R.(Prim (Plus (e1, Prim (Minus e2)))) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Subtract (a1, a2)))], Var x)
  | R.(Prim (Plus (Prim (Minus e1), e2))) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Subtract (a2, a1)))], Var x)
  | R.(Prim (Plus (e1, e2))) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Plus (a1, a2)))], Var x)
  | R.(Prim (Subtract (e1, e2))) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Subtract (a1, a2)))], Var x)
  | R.(Prim (Mult (e, Int -1))) | R.(Prim (Mult (Int -1, e))) ->
      let nv, a = resolve_complex_exp m n e in
      let x = fresh_var n in
      (nv @ [(x, Prim (Minus a))], Var x)
  | R.(Prim (Mult (e1, e2))) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Mult (a1, a2)))], Var x)
  | R.(Prim (Div (e1, e2))) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Div (a1, a2)))], Var x)
  | R.(Prim (Rem (e1, e2))) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Rem (a1, a2)))], Var x)
  | R.(Prim (Land (e1, e2))) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Land (a1, a2)))], Var x)
  | R.(Prim (Lor (e1, e2))) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Lor (a1, a2)))], Var x)
  | R.(Prim (Lxor (e1, e2))) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Lxor (a1, a2)))], Var x)
  | R.(Prim (Lnot e)) ->
      let nv, a = resolve_complex_exp m n e in
      let x = fresh_var n in
      (nv @ [(x, Prim (Lnot a))], Var x)
  | R.(Prim (Eq (e1, e2))) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Eq (a1, a2)))], Var x)
  | R.(Prim (Lt (e1, e2))) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Lt (a1, a2)))], Var x)
  | R.(Prim (Le (e1, e2))) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Le (a1, a2)))], Var x)
  | R.(Prim (Gt (e1, e2))) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Gt (a1, a2)))], Var x)
  | R.(Prim (Ge (e1, e2))) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Ge (a1, a2)))], Var x)
  | R.(Prim (Not e)) ->
      let nv, a = resolve_complex_exp m n e in
      let x = fresh_var n in
      (nv @ [(x, Prim (Not a))], Var x)
  | R.(Prim (And (e1, e2))) ->
      let e =
        If (allow_complex m n e1, allow_complex m n e2, Atom (Bool false))
      in
      let x = fresh_var n in
      ([(x, e)], Var x)
  | R.(Prim (Or (e1, e2))) ->
      let e =
        If (allow_complex m n e1, Atom (Bool true), allow_complex m n e2)
      in
      let x = fresh_var n in
      ([(x, e)], Var x)
  | R.Var v -> (
    match Map.find m v with
    | None -> failwith ("R_anf.resolve_complex_exp: var " ^ v ^ " is unbound")
    | Some e -> ([], e) )
  | R.Let (v, e1, e2) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let m' = Map.set m v a1 in
      let nv2, a2 = resolve_complex_exp m' n e2 in
      (nv1 @ nv2, a2)
  | R.If (e1, e2, e3) ->
      let e =
        If (allow_complex m n e1, allow_complex m n e2, allow_complex m n e3)
      in
      let x = fresh_var n in
      ([(x, e)], Var x)

and allow_complex m n = function
  | R.Int i -> Atom (Int i)
  | R.Bool b -> Atom (Bool b)
  | R.Prim Read -> Prim Read
  | R.Prim (Minus e) ->
      let nv, a = resolve_complex_exp m n e in
      unfold nv (Prim (Minus a))
  | R.Prim (Plus (e1, e2)) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      unfold (nv1 @ nv2) (Prim (Plus (a1, a2)))
  | R.Prim (Subtract (e1, e2)) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      unfold (nv1 @ nv2) (Prim (Subtract (a1, a2)))
  | R.Prim (Mult (e1, e2)) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      unfold (nv1 @ nv2) (Prim (Mult (a1, a2)))
  | R.Prim (Div (e1, e2)) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      unfold (nv1 @ nv2) (Prim (Div (a1, a2)))
  | R.Prim (Rem (e1, e2)) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      unfold (nv1 @ nv2) (Prim (Rem (a1, a2)))
  | R.Prim (Land (e1, e2)) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      unfold (nv1 @ nv2) (Prim (Land (a1, a2)))
  | R.Prim (Lor (e1, e2)) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      unfold (nv1 @ nv2) (Prim (Lor (a1, a2)))
  | R.Prim (Lxor (e1, e2)) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      unfold (nv1 @ nv2) (Prim (Lxor (a1, a2)))
  | R.Prim (Lnot e) ->
      let nv, a = resolve_complex_exp m n e in
      unfold nv (Prim (Lnot a))
  | R.Prim (Eq (e1, e2)) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      unfold (nv1 @ nv2) (Prim (Eq (a1, a2)))
  | R.Prim (Lt (e1, e2)) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      unfold (nv1 @ nv2) (Prim (Lt (a1, a2)))
  | R.Prim (Le (e1, e2)) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      unfold (nv1 @ nv2) (Prim (Le (a1, a2)))
  | R.Prim (Gt (e1, e2)) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      unfold (nv1 @ nv2) (Prim (Gt (a1, a2)))
  | R.Prim (Ge (e1, e2)) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      unfold (nv1 @ nv2) (Prim (Ge (a1, a2)))
  | R.Prim (Not e) ->
      let l, a = resolve_complex_exp m n e in
      unfold l (Prim (Not a))
  | R.Prim (And (e1, e2)) ->
      If (allow_complex m n e1, allow_complex m n e2, Atom (Bool false))
  | R.Prim (Or (e1, e2)) ->
      If (allow_complex m n e1, Atom (Bool true), allow_complex m n e2)
  | R.Var v -> (
    match Map.find m v with
    | None -> failwith ("R_anf.resolve_complex_exp: var " ^ v ^ " is unbound")
    | Some e -> Atom e )
  | R.Let (v, e1, e2) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let m' = Map.set m v a1 in
      let nv2, a2 = resolve_complex_exp m' n e2 in
      unfold (nv1 @ nv2) (Atom a2)
  | R.If (e1, e2, e3) ->
      If (allow_complex m n e1, allow_complex m n e2, allow_complex m n e3)

and fresh_var n =
  let x = Printf.sprintf "%%%d" !n in
  incr n; x

and unfold nv e =
  List.fold_right nv ~init:e ~f:(fun (v, e) acc -> Let (v, e, acc))

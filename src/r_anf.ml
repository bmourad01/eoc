open Core_kernel

type info = R.info

type var = R.var

type t = Program of info * exp

and exp = Atom of atom | Prim of prim | Let of var * exp * exp

and atom = Int of int | Var of var

and prim = Read | Minus of atom | Plus of atom * atom

let rec to_string = function
  | Program (_, e) -> string_of_exp e

and string_of_exp = function
  | Atom a -> string_of_atom a
  | Prim p -> string_of_prim p
  | Let (v, e1, e2) ->
      Printf.sprintf "(let ([%s %s]) %s)" v (string_of_exp e1)
        (string_of_exp e2)

and string_of_atom = function
  | Int i -> Int.to_string i
  | Var v -> v

and string_of_prim = function
  | Read -> "(read)"
  | Minus a -> Printf.sprintf "(- %s)" (string_of_atom a)
  | Plus (a1, a2) ->
      Printf.sprintf "(+ %s %s)" (string_of_atom a1) (string_of_atom a2)

let rec resolve_complex = function
  | R.Program (info, e) ->
      let l, a = resolve_complex_exp R.empty_var_env (ref 1) e in
      let e =
        List.fold_right l ~init:(Atom a) ~f:(fun (v, e) acc ->
            Let (v, e, acc))
      in
      Program (info, e)

and resolve_complex_exp m n = function
  | R.Int i -> ([], Int i)
  | R.(Prim Read) ->
      let x = fresh_var n in
      ([(x, Prim Read)], Var x)
  | R.(Prim (Minus e)) ->
      let nv, a = resolve_complex_exp m n e in
      let x = fresh_var n in
      (nv @ [(x, Prim (Minus a))], Var x)
  | R.(Prim (Plus (e1, e2))) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let nv2, a2 = resolve_complex_exp m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Plus (a1, a2)))], Var x)
  | R.Var v -> (
    match Map.find m v with
    | None -> failwith ("R_anf.resolve_complex_exp: var " ^ v ^ " is unbound")
    | Some e -> ([], e) )
  | R.Let (v, e1, e2) ->
      let nv1, a1 = resolve_complex_exp m n e1 in
      let m' = Map.set m v a1 in
      let nv2, a2 = resolve_complex_exp m' n e2 in
      (nv1 @ nv2, a2)

and fresh_var n =
  let x = Printf.sprintf "%%%d" !n in
  n := !n + 1;
  x

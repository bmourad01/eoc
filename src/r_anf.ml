open Core_kernel

type var = R_alloc.var

module Type = R_alloc.Type

type info = {typ: Type.t}

type t = Program of info * exp

and exp =
  | Atom of atom
  | Prim of prim
  | Let of var * exp * exp
  | If of exp * exp * exp
  | Collect of int
  | Allocate of int * Type.t
  | Globalvalue of string

and atom =
  | Int of int
  | Bool of bool
  | Var of var
  | Void
  | Hastype of exp * Type.t

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

let rec to_string ?(has_type = false) = function
  | Program (_, e) -> string_of_exp e ~has_type

and string_of_exp ?(has_type = false) = function
  | Atom a -> string_of_atom a ~has_type
  | Prim p -> string_of_prim p ~has_type
  | Let (v, e1, e2) ->
      Printf.sprintf "(let ([%s %s]) %s)" v
        (string_of_exp e1 ~has_type)
        (string_of_exp e2 ~has_type)
  | If (e1, e2, e3) ->
      Printf.sprintf "(if %s %s %s)"
        (string_of_exp e1 ~has_type)
        (string_of_exp e2 ~has_type)
        (string_of_exp e3 ~has_type)
  | Collect n -> Printf.sprintf "(collect %d)" n
  | Allocate (n, t) -> Printf.sprintf "(allocate %d %s)" n (Type.to_string t)
  | Globalvalue v -> Printf.sprintf "(global-value '%s)" v

and string_of_atom ?(has_type = false) = function
  | Int i -> Int.to_string i
  | Bool b -> if b then "#t" else "#f"
  | Var v -> v
  | Void -> "(void)"
  | Hastype (e, t) ->
      if has_type then
        Printf.sprintf "(has-type %s %s)"
          (string_of_exp e ~has_type)
          (Type.to_string t)
      else string_of_exp e ~has_type

and string_of_prim ?(has_type = false) = function
  | Read -> "(read)"
  | Minus a -> Printf.sprintf "(- %s)" (string_of_atom a ~has_type)
  | Plus (a1, a2) ->
      Printf.sprintf "(+ %s %s)"
        (string_of_atom a1 ~has_type)
        (string_of_atom a2 ~has_type)
  | Subtract (a1, a2) ->
      Printf.sprintf "(- %s %s)"
        (string_of_atom a1 ~has_type)
        (string_of_atom a2 ~has_type)
  | Mult (a1, a2) ->
      Printf.sprintf "(* %s %s)"
        (string_of_atom a1 ~has_type)
        (string_of_atom a2 ~has_type)
  | Div (a1, a2) ->
      Printf.sprintf "(/ %s %s)"
        (string_of_atom a1 ~has_type)
        (string_of_atom a2 ~has_type)
  | Rem (a1, a2) ->
      Printf.sprintf "(rem %s %s)"
        (string_of_atom a1 ~has_type)
        (string_of_atom a2 ~has_type)
  | Land (a1, a2) ->
      Printf.sprintf "(land %s %s)"
        (string_of_atom a1 ~has_type)
        (string_of_atom a2 ~has_type)
  | Lor (a1, a2) ->
      Printf.sprintf "(lor %s %s)"
        (string_of_atom a1 ~has_type)
        (string_of_atom a2 ~has_type)
  | Lxor (a1, a2) ->
      Printf.sprintf "(lxor %s %s)"
        (string_of_atom a1 ~has_type)
        (string_of_atom a2 ~has_type)
  | Lnot a -> Printf.sprintf "(lnot %s)" (string_of_atom a ~has_type)
  | Eq (a1, a2) ->
      Printf.sprintf "(eq? %s %s)"
        (string_of_atom a1 ~has_type)
        (string_of_atom a2 ~has_type)
  | Lt (a1, a2) ->
      Printf.sprintf "(< %s %s)"
        (string_of_atom a1 ~has_type)
        (string_of_atom a2 ~has_type)
  | Le (a1, a2) ->
      Printf.sprintf "(<= %s %s)"
        (string_of_atom a1 ~has_type)
        (string_of_atom a2 ~has_type)
  | Gt (a1, a2) ->
      Printf.sprintf "(> %s %s)"
        (string_of_atom a1 ~has_type)
        (string_of_atom a2 ~has_type)
  | Ge (a1, a2) ->
      Printf.sprintf "(>= %s %s)"
        (string_of_atom a1 ~has_type)
        (string_of_atom a2 ~has_type)
  | Not a -> Printf.sprintf "(not %s)" (string_of_atom a ~has_type)
  | Vectorlength a ->
      Printf.sprintf "(vector-length %s)" (string_of_atom a ~has_type)
  | Vectorref (a, i) ->
      Printf.sprintf "(vector-ref %s %d)" (string_of_atom a ~has_type) i
  | Vectorset (a1, i, a2) ->
      Printf.sprintf "(vector-set! %s %d %s)"
        (string_of_atom a1 ~has_type)
        i
        (string_of_atom a2 ~has_type)

let rec resolve_complex = function
  | R_alloc.Program (info, e) ->
      let typeenv = Hashtbl.create (module String) in
      let e =
        resolve_complex_exp typeenv R.empty_var_env (ref info.nvars) e
      in
      Program ({typ= info.typ}, e)

and resolve_complex_atom typeenv m n = function
  | R_alloc.Int i -> ([], Int i)
  | R_alloc.Bool b -> ([], Bool b)
  | R_alloc.Void -> ([], Void)
  | R_alloc.(Prim Read) ->
      let x = fresh_var n in
      ([(x, Prim Read)], Var x)
  | R_alloc.(Prim (Minus e)) ->
      let nv, a = resolve_complex_atom typeenv m n e in
      let x = fresh_var n in
      (nv @ [(x, Prim (Minus a))], Var x)
  | R_alloc.(Prim (Plus (e1, Prim (Minus e2)))) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Subtract (a1, a2)))], Var x)
  | R_alloc.(Prim (Plus (Prim (Minus e1), e2))) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Subtract (a2, a1)))], Var x)
  | R_alloc.(Prim (Plus (e1, e2))) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Plus (a1, a2)))], Var x)
  | R_alloc.(Prim (Subtract (e1, e2))) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Subtract (a1, a2)))], Var x)
  | R_alloc.(Prim (Mult (e, Int -1))) | R_alloc.(Prim (Mult (Int -1, e))) ->
      let nv, a = resolve_complex_atom typeenv m n e in
      let x = fresh_var n in
      (nv @ [(x, Prim (Minus a))], Var x)
  | R_alloc.(Prim (Mult (e1, e2))) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Mult (a1, a2)))], Var x)
  | R_alloc.(Prim (Div (e1, e2))) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Div (a1, a2)))], Var x)
  | R_alloc.(Prim (Rem (e1, e2))) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Rem (a1, a2)))], Var x)
  | R_alloc.(Prim (Land (e1, e2))) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Land (a1, a2)))], Var x)
  | R_alloc.(Prim (Lor (e1, e2))) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Lor (a1, a2)))], Var x)
  | R_alloc.(Prim (Lxor (e1, e2))) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Lxor (a1, a2)))], Var x)
  | R_alloc.(Prim (Lnot e)) ->
      let nv, a = resolve_complex_atom typeenv m n e in
      let x = fresh_var n in
      (nv @ [(x, Prim (Lnot a))], Var x)
  | R_alloc.(Prim (Eq (e1, e2))) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Eq (a1, a2)))], Var x)
  | R_alloc.(Prim (Lt (e1, e2))) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Lt (a1, a2)))], Var x)
  | R_alloc.(Prim (Le (e1, e2))) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Le (a1, a2)))], Var x)
  | R_alloc.(Prim (Gt (e1, e2))) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Gt (a1, a2)))], Var x)
  | R_alloc.(Prim (Ge (e1, e2))) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Ge (a1, a2)))], Var x)
  | R_alloc.(Prim (Not e)) ->
      let nv, a = resolve_complex_atom typeenv m n e in
      let x = fresh_var n in
      (nv @ [(x, Prim (Not a))], Var x)
  | R_alloc.(Prim (And (e1, e2))) ->
      let e =
        If
          ( resolve_complex_exp typeenv m n e1
          , resolve_complex_exp typeenv m n e2
          , Atom (Bool false) )
      in
      let x = fresh_var n in
      ([(x, e)], Var x)
  | R_alloc.(Prim (Or (e1, e2))) ->
      let e =
        If
          ( resolve_complex_exp typeenv m n e1
          , Atom (Bool true)
          , resolve_complex_exp typeenv m n e2 )
      in
      let x = fresh_var n in
      ([(x, e)], Var x)
  | R_alloc.(Prim (Vectorlength e)) ->
      let nv, a = resolve_complex_atom typeenv m n e in
      let x = fresh_var n in
      (nv @ [(x, Prim (Vectorlength a))], Var x)
  | R_alloc.(Prim (Vectorref (e, i))) ->
      let nv, a = resolve_complex_atom typeenv m n e in
      let x = fresh_var n in
      (nv @ [(x, Prim (Vectorref (a, i)))], Var x)
  | R_alloc.(Prim (Vectorset (e1, i, e2))) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      let x = fresh_var n in
      (nv1 @ nv2 @ [(x, Prim (Vectorset (a1, i, a2)))], Var x)
  | R_alloc.Var v -> (
    match Map.find m v with
    | None ->
        failwith ("R_anf.resolve_complex_atom: var " ^ v ^ " is unbound")
    | Some e -> ([], e) )
  | R_alloc.Let (v, e1, e2) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let m' = Map.set m v a1 in
      let nv2, a2 = resolve_complex_atom typeenv m' n e2 in
      (nv1 @ nv2, a2)
  | R_alloc.If (e1, e2, e3) ->
      let e =
        If
          ( resolve_complex_exp typeenv m n e1
          , resolve_complex_exp typeenv m n e2
          , resolve_complex_exp typeenv m n e3 )
      in
      let x = fresh_var n in
      ([(x, e)], Var x)
  | R_alloc.Hastype (e, t) ->
      let e = resolve_complex_exp typeenv m n e in
      let x = fresh_var n in
      Hashtbl.set typeenv x t;
      ([(x, e)], Var x)
  | R_alloc.Collect n' ->
      let e = Collect n' in
      let x = fresh_var n in
      ([(x, e)], Var x)
  | R_alloc.Allocate (n', t) ->
      let e = Allocate (n', t) in
      let x = fresh_var n in
      ([(x, e)], Var x)
  | R_alloc.Globalvalue v ->
      let e = Globalvalue v in
      let x = fresh_var n in
      ([(x, e)], Var x)

and resolve_complex_exp typeenv m n = function
  | R_alloc.Int i -> Atom (Int i)
  | R_alloc.Bool b -> Atom (Bool b)
  | R_alloc.Void -> Atom Void
  | R_alloc.Prim Read -> Prim Read
  (* XXX: this is ugly *)
  | R_alloc.(Hastype (Prim (Minus e), t)) ->
      let nv, a = resolve_complex_atom typeenv m n e in
      unfold typeenv nv (Atom (Hastype (Prim (Minus a), t)))
  | R_alloc.(Hastype (Prim (Plus (e1, e2)), t)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Atom (Hastype (Prim (Plus (a1, a2)), t)))
  | R_alloc.(Hastype (Prim (Subtract (e1, e2)), t)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2)
        (Atom (Hastype (Prim (Subtract (a1, a2)), t)))
  | R_alloc.(Hastype (Prim (Mult (e1, e2)), t)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Atom (Hastype (Prim (Mult (a1, a2)), t)))
  | R_alloc.(Hastype (Prim (Div (e1, e2)), t)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Atom (Hastype (Prim (Div (a1, a2)), t)))
  | R_alloc.(Hastype (Prim (Rem (e1, e2)), t)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Atom (Hastype (Prim (Rem (a1, a2)), t)))
  | R_alloc.(Hastype (Prim (Land (e1, e2)), t)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Atom (Hastype (Prim (Land (a1, a2)), t)))
  | R_alloc.(Hastype (Prim (Lor (e1, e2)), t)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Atom (Hastype (Prim (Lor (a1, a2)), t)))
  | R_alloc.(Hastype (Prim (Lxor (e1, e2)), t)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Atom (Hastype (Prim (Lxor (a1, a2)), t)))
  | R_alloc.(Hastype (Prim (Lnot e), t)) ->
      let nv, a = resolve_complex_atom typeenv m n e in
      unfold typeenv nv (Atom (Hastype (Prim (Lnot a), t)))
  | R_alloc.(Hastype (Prim (Eq (e1, e2)), t)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Atom (Hastype (Prim (Eq (a1, a2)), t)))
  | R_alloc.(Hastype (Prim (Lt (e1, e2)), t)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Atom (Hastype (Prim (Lt (a1, a2)), t)))
  | R_alloc.(Hastype (Prim (Le (e1, e2)), t)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Atom (Hastype (Prim (Le (a1, a2)), t)))
  | R_alloc.(Hastype (Prim (Gt (e1, e2)), t)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Atom (Hastype (Prim (Gt (a1, a2)), t)))
  | R_alloc.(Hastype (Prim (Ge (e1, e2)), t)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Atom (Hastype (Prim (Ge (a1, a2)), t)))
  | R_alloc.(Hastype (Prim (Not e), t)) ->
      let nv, a = resolve_complex_atom typeenv m n e in
      unfold typeenv nv (Atom (Hastype (Prim (Not a), t)))
  | R_alloc.(Hastype (Prim (Vectorlength e), t)) ->
      let nv, a = resolve_complex_atom typeenv m n e in
      unfold typeenv nv (Atom (Hastype (Prim (Vectorlength a), t)))
  | R_alloc.(Hastype (Prim (Vectorref (e, i)), t)) ->
      let nv, a = resolve_complex_atom typeenv m n e in
      unfold typeenv nv (Atom (Hastype (Prim (Vectorref (a, i)), t)))
  | R_alloc.(Hastype (Prim (Vectorset (e1, i, e2)), t)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2)
        (Atom (Hastype (Prim (Vectorset (a1, i, a2)), t)))
  | R_alloc.Prim (Minus e) ->
      let nv, a = resolve_complex_atom typeenv m n e in
      unfold typeenv nv (Prim (Minus a))
  | R_alloc.Prim (Plus (e1, e2)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Prim (Plus (a1, a2)))
  | R_alloc.Prim (Subtract (e1, e2)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Prim (Subtract (a1, a2)))
  | R_alloc.Prim (Mult (e1, e2)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Prim (Mult (a1, a2)))
  | R_alloc.Prim (Div (e1, e2)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Prim (Div (a1, a2)))
  | R_alloc.Prim (Rem (e1, e2)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Prim (Rem (a1, a2)))
  | R_alloc.Prim (Land (e1, e2)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Prim (Land (a1, a2)))
  | R_alloc.Prim (Lor (e1, e2)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Prim (Lor (a1, a2)))
  | R_alloc.Prim (Lxor (e1, e2)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Prim (Lxor (a1, a2)))
  | R_alloc.Prim (Lnot e) ->
      let nv, a = resolve_complex_atom typeenv m n e in
      unfold typeenv nv (Prim (Lnot a))
  | R_alloc.Prim (Eq (e1, e2)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Prim (Eq (a1, a2)))
  | R_alloc.Prim (Lt (e1, e2)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Prim (Lt (a1, a2)))
  | R_alloc.Prim (Le (e1, e2)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Prim (Le (a1, a2)))
  | R_alloc.Prim (Gt (e1, e2)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Prim (Gt (a1, a2)))
  | R_alloc.Prim (Ge (e1, e2)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Prim (Ge (a1, a2)))
  | R_alloc.Prim (Not e) ->
      let nv, a = resolve_complex_atom typeenv m n e in
      unfold typeenv nv (Prim (Not a))
  | R_alloc.Prim (And (e1, e2)) ->
      If
        ( resolve_complex_exp typeenv m n e1
        , resolve_complex_exp typeenv m n e2
        , Atom (Bool false) )
  | R_alloc.Prim (Or (e1, e2)) ->
      If
        ( resolve_complex_exp typeenv m n e1
        , Atom (Bool true)
        , resolve_complex_exp typeenv m n e2 )
  | R_alloc.Prim (Vectorlength e) ->
      let nv, a = resolve_complex_atom typeenv m n e in
      unfold typeenv nv (Prim (Vectorlength a))
  | R_alloc.Prim (Vectorref (e, i)) ->
      let nv, a = resolve_complex_atom typeenv m n e in
      unfold typeenv nv (Prim (Vectorref (a, i)))
  | R_alloc.Prim (Vectorset (e1, i, e2)) ->
      let nv1, a1 = resolve_complex_atom typeenv m n e1 in
      let nv2, a2 = resolve_complex_atom typeenv m n e2 in
      unfold typeenv (nv1 @ nv2) (Prim (Vectorset (a1, i, a2)))
  | R_alloc.Var v -> (
    match Map.find m v with
    | None -> failwith ("R_anf.resolve_complex_exp: var " ^ v ^ " is unbound")
    | Some e -> Atom e )
  | R_alloc.Let (v, e1, e2) ->
      let x = fresh_var n in
      Let
        ( x
        , resolve_complex_exp typeenv m n e1
        , resolve_complex_exp typeenv (Map.set m v (Var x)) n e2 )
  | R_alloc.If (e1, e2, e3) ->
      If
        ( resolve_complex_exp typeenv m n e1
        , resolve_complex_exp typeenv m n e2
        , resolve_complex_exp typeenv m n e3 )
  | R_alloc.Hastype (e, t) ->
      Atom (Hastype (resolve_complex_exp typeenv m n e, t))
  | R_alloc.Collect n -> Collect n
  | R_alloc.Allocate (n, t) -> Allocate (n, t)
  | R_alloc.Globalvalue v -> Globalvalue v

and fresh_var n =
  let x = Printf.sprintf "%%%d" !n in
  incr n; x

and unfold typeenv nv e =
  List.fold_right nv ~init:e ~f:(fun (v, e) acc ->
      let e =
        match Hashtbl.find typeenv v with
        | None -> e
        | Some t -> Atom (Hastype (e, t))
      in
      Let (v, e, acc))

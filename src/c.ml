open Core_kernel

type label = string

type 'a label_map = 'a String.Map.t

let empty_label_map = String.Map.empty

module Type = R.Type

type type_env = R.type_env

module Cfg = Graph.Persistent.Digraph.Concrete (String)

type info = {main: label; typ: Type.t; cfg: Cfg.t}

type var = R.var

module Cmp = struct
  type t = Eq | Lt | Le | Gt | Ge

  let to_string = function
    | Eq -> "eq?"
    | Lt -> "<"
    | Le -> "<="
    | Gt -> ">"
    | Ge -> ">="
end

type t = Program of info * tails

and tails = (label * tail) list

and tail =
  | Return of exp
  | Seq of stmt * tail
  | Goto of label
  | If of cmp * label * label

and stmt = Assign of var * exp

and exp = Atom of atom | Prim of prim

and atom = R_anf.atom

and prim = R_anf.prim

and cmp = Cmp.t * atom * atom

let rec to_string = function
  | Program (_, tails) ->
      let tls =
        List.map tails ~f:(fun (l, t) ->
            Printf.sprintf "(%s:\n  %s)" l (string_of_tail t))
        |> String.concat ~sep:"\n "
      in
      Printf.sprintf "(%s)" tls

and string_of_tail = function
  | Return e -> Printf.sprintf "(return %s)" (string_of_exp e)
  | Seq (s, t) ->
      Printf.sprintf "%s\n  %s" (string_of_stmt s) (string_of_tail t)
  | Goto l -> Printf.sprintf "(goto %s)" l
  | If (cmp, lt, lf) ->
      Printf.sprintf "(if %s (goto %s) (goto %s))" (string_of_cmp cmp) lt lf

and string_of_stmt = function
  | Assign (v, e) -> Printf.sprintf "(set! %s %s)" v (string_of_exp e)

and string_of_exp = function
  | Atom a -> string_of_atom a
  | Prim p -> string_of_prim p

and string_of_atom = R_anf.string_of_atom

and string_of_prim = R_anf.string_of_prim

and string_of_cmp (cmp, a1, a2) =
  Printf.sprintf "(%s %s %s)" (Cmp.to_string cmp) (string_of_atom a1)
    (string_of_atom a2)

let read_int () =
  Out_channel.(flush stdout);
  Int.of_string In_channel.(input_line_exn stdin)

type answer = R.answer

let rec interp ?(read = None) = function
  | Program (info, tails) -> (
      let tails = String.Map.of_alist_exn tails in
      match Map.find tails info.main with
      | None -> failwith "C.interp: no main label defined"
      | Some t -> interp_tail R.empty_var_env tails t ~read )

and interp_tail ?(read = None) env tails = function
  | Return e -> interp_exp env e ~read
  | Seq (s, t) ->
      let env = interp_stmt env s ~read in
      interp_tail env tails t ~read
  | Goto l -> (
    match Map.find tails l with
    | None -> failwith ("C.interp: goto label " ^ l ^ " does not exist")
    | Some t -> interp_tail env tails t ~read )
  | If (cmp, lt, lf) -> (
    match interp_cmp env cmp with
    | `Bool b ->
        let t = if b then Goto lt else Goto lf in
        interp_tail env tails t ~read
    | _ -> assert false )

and interp_stmt ?(read = None) env = function
  | Assign (v, e) ->
      let e = interp_exp env e ~read in
      Map.set env v e

and interp_exp ?(read = None) env = function
  | Atom a -> interp_atom env a
  | Prim p -> interp_prim env p ~read

and interp_atom env = function
  | Int i -> `Int i
  | Bool b -> `Bool b
  | Var v -> (
    match Map.find env v with
    | None -> failwith ("C.interp_atom: var " ^ v ^ " is unbound")
    | Some i -> i )

and interp_prim ?(read = None) env = function
  | Read -> (
    match read with
    | Some i -> `Int i
    | None -> `Int (read_int ()) )
  | Minus a -> (
    match interp_atom env a with
    | `Int i -> `Int (-i)
    | _ -> assert false )
  | Plus (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Int (i1 + i2)
    | _ -> assert false )
  | Subtract (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Int (i1 - i2)
    | _ -> assert false )
  | Mult (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Int (i1 * i2)
    | _ -> assert false )
  | Eq (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Bool (i1 = i2)
    | `Bool b1, `Bool b2 -> `Bool (Bool.equal b1 b2)
    | _ -> assert false )
  | Lt (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Bool (i1 < i2)
    | _ -> assert false )
  | Le (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Bool (i1 <= i2)
    | _ -> assert false )
  | Gt (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Bool (i1 > i2)
    | _ -> assert false )
  | Ge (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Bool (i1 >= i2)
    | _ -> assert false )
  | Not a -> (
    match interp_atom env a with
    | `Bool b -> `Bool (not b)
    | _ -> assert false )

and interp_cmp ?(read = None) env (cmp, a1, a2) =
  let e =
    match cmp with
    | Cmp.Eq -> Prim (Eq (a1, a2))
    | Cmp.Lt -> Prim (Lt (a1, a2))
    | Cmp.Le -> Prim (Le (a1, a2))
    | Cmp.Gt -> Prim (Gt (a1, a2))
    | Cmp.Ge -> Prim (Ge (a1, a2))
  in
  interp_exp env e ~read

let start_label = "main"

let rec explicate_control = function
  | R_anf.Program (info, e) ->
      let cfg = Cfg.(add_vertex empty start_label) in
      let info = {main= start_label; typ= info.typ; cfg} in
      let tails = ref empty_label_map in
      let tail = explicate_tail tails (ref 0) e in
      let tails = Map.set !tails start_label tail in
      let cfg =
        Map.fold tails ~init:cfg ~f:(fun ~key:label ~data:tail cfg ->
            let rec aux = function
              | Return _ -> cfg
              | Seq (_, t) -> aux t
              | Goto l -> Cfg.add_edge cfg label l
              | If (_, lt, lf) ->
                  Cfg.(add_edge (add_edge cfg label lt) label lf)
            in
            aux tail)
      in
      Program ({info with cfg}, Map.to_alist tails |> List.rev)

and explicate_tail tails n = function
  | R_anf.(Atom a) -> Return (Atom a)
  | R_anf.(Prim p) -> Return (Prim p)
  | R_anf.(Let (v, e1, e2)) ->
      let cont = explicate_tail tails n e2 in
      explicate_assign tails n e1 v cont
  | R_anf.(If (e1, e2, e3)) ->
      let tt = explicate_tail tails n e2 in
      let tf = explicate_tail tails n e3 in
      explicate_pred tails n e1 tt tf

and explicate_assign tails n e x cont =
  match e with
  | R_anf.(Let (v, e1, e2)) ->
      let cont = explicate_assign tails n e2 x cont in
      explicate_assign tails n e1 v cont
  | R_anf.(If (e1, e2, e3)) ->
      let tt = explicate_assign tails n e2 x cont in
      let tf = explicate_assign tails n e3 x cont in
      explicate_pred tails n e1 tt tf
  | _ ->
      let t = explicate_tail tails n e in
      do_assign t x cont

and explicate_pred tails n cnd thn els =
  match cnd with
  | R_anf.(Atom (Bool b)) -> if b then thn else els
  | R_anf.(Atom (Var x)) ->
      let lt = fresh_label n in
      let lf = fresh_label n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Eq, Var x, Bool true), lt, lf)
  | R_anf.(Prim (Not a)) ->
      let lt = fresh_label n in
      let lf = fresh_label n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Eq, a, Bool false), lt, lf)
  | R_anf.(Prim (Eq (a1, a2))) ->
      let lt = fresh_label n in
      let lf = fresh_label n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Eq, a1, a2), lt, lf)
  | R_anf.(Prim (Lt (a1, a2))) ->
      let lt = fresh_label n in
      let lf = fresh_label n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Lt, a1, a2), lt, lf)
  | R_anf.(Prim (Le (a1, a2))) ->
      let lt = fresh_label n in
      let lf = fresh_label n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Le, a1, a2), lt, lf)
  | R_anf.(Prim (Gt (a1, a2))) ->
      let lt = fresh_label n in
      let lf = fresh_label n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Gt, a1, a2), lt, lf)
  | R_anf.(Prim (Ge (a1, a2))) ->
      let lt = fresh_label n in
      let lf = fresh_label n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Ge, a1, a2), lt, lf)
  | R_anf.(Let (x, e1, e2)) ->
      let t = explicate_pred tails n e2 thn els in
      explicate_assign tails n e1 x t
  | R_anf.(If (e1, e2, e3)) ->
      let lt = fresh_label n in
      let lf = fresh_label n in
      add_tail tails lt thn;
      add_tail tails lf els;
      let tt = explicate_pred tails n e2 (Goto lt) (Goto lf) in
      let tf = explicate_pred tails n e3 (Goto lt) (Goto lf) in
      explicate_pred tails n e1 tt tf
  | _ -> assert false

and do_assign e x cont =
  match e with
  | Return (Atom a) -> Seq (Assign (x, Atom a), cont)
  | Return (Prim p) -> Seq (Assign (x, Prim p), cont)
  | Seq (s, t) -> Seq (s, do_assign t x cont)
  | _ -> failwith ("C.do_assign: unhandled case " ^ string_of_tail e)

and fresh_label n =
  let l = Printf.sprintf ".L%d" !n in
  incr n; l

and add_tail tails l t = tails := Map.set !tails l t

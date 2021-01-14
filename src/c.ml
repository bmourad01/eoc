open Core_kernel

type label = string

type 'a label_map = 'a String.Map.t

let empty_label_map = String.Map.empty

type info = {main: label; locals_types: R.type_env}

type var = R.var

type t = Program of info * tails

and tails = tail label_map

and tail = Return of exp | Seq of stmt * tail

and stmt = Assign of var * exp

and exp = Atom of atom | Prim of prim

and atom = R_anf.atom

and prim = R_anf.prim

let rec to_string = function
  | Program (_, tails) ->
      let tls =
        Map.to_alist tails
        |> List.map ~f:(fun (l, t) ->
               Printf.sprintf "(%s:\n  %s)" l (string_of_tail t))
        |> String.concat ~sep:"\n"
      in
      Printf.sprintf "(%s)" tls

and string_of_tail = function
  | Return e -> Printf.sprintf "(return %s)" (string_of_exp e)
  | Seq (s, t) ->
      Printf.sprintf "%s\n  %s" (string_of_stmt s) (string_of_tail t)

and string_of_stmt = function
  | Assign (v, e) -> Printf.sprintf "(set! %s %s)" v (string_of_exp e)

and string_of_exp = function
  | Atom a -> string_of_atom a
  | Prim p -> string_of_prim p

and string_of_atom = function
  | Int i -> Int.to_string i
  | Var v -> v

and string_of_prim = function
  | Read -> "(read)"
  | Minus a -> Printf.sprintf "(- %s)" (string_of_atom a)
  | Plus (a1, a2) ->
      Printf.sprintf "(+ %s %s)" (string_of_atom a1) (string_of_atom a2)

let read_int () =
  Out_channel.(flush stdout);
  Int.of_string In_channel.(input_line_exn stdin)

let rec interp ?(read = None) = function
  | Program (info, tails) -> (
    match Map.find tails info.main with
    | None -> failwith "C.interp: no main label defined"
    | Some t -> interp_tail R.empty_var_env t ~read )

and interp_tail ?(read = None) env = function
  | Return e -> interp_exp env e ~read
  | Seq (s, t) ->
      let env = interp_stmt env s ~read in
      interp_tail env t ~read

and interp_stmt ?(read = None) env = function
  | Assign (v, e) ->
      let e = interp_exp env e ~read in
      Map.set env v e

and interp_exp ?(read = None) env = function
  | Atom a -> interp_atom env a
  | Prim p -> interp_prim env p ~read

and interp_atom env = function
  | Int i -> i
  | Var v -> (
    match Map.find env v with
    | None -> failwith ("C.interp_atom: var " ^ v ^ " is unbound")
    | Some i -> i )

and interp_prim ?(read = None) env = function
  | Read -> (
    match read with
    | Some i -> i
    | None -> read_int () )
  | Minus a -> interp_atom env a
  | Plus (a1, a2) -> interp_atom env a1 + interp_atom env a2

let start_label = "_start"

let rec type_check_cvar = function
  | Program (info, tails) ->
      let locals_types =
        Map.fold tails ~init:empty_label_map ~f:(fun ~key:_ ~data:tail acc ->
            type_check_cvar_tail acc tail)
      in
      Program ({info with locals_types}, tails)

and type_check_cvar_tail env = function
  | Return e ->
      let _ = type_check_cvar_exp env e in
      env
  | Seq (s, t) ->
      let env = type_check_cvar_stmt env s in
      type_check_cvar_tail env t

and type_check_cvar_stmt env = function
  | Assign (v, e) ->
      let typ = type_check_cvar_exp env e in
      Map.set env v typ

and type_check_cvar_exp env = function
  | Atom a -> type_check_cvar_atom env a
  | Prim p -> type_check_cvar_prim env p

and type_check_cvar_atom env = function
  | Int _ -> R.Type.Integer
  | Var v -> (
    match Map.find env v with
    | None -> failwith ("C.type_check_cvar_atom: var " ^ v ^ " is unbound")
    | Some typ -> typ )

and type_check_cvar_prim env = function
  | Read -> Integer
  | Minus a -> (
    match type_check_cvar_atom env a with
    | Integer -> Integer )
  | Plus (a1, a2) -> (
    match (type_check_cvar_atom env a1, type_check_cvar_atom env a2) with
    | Integer, Integer -> Integer )

let rec explicate_control = function
  | R_anf.Program (_, e) ->
      let info = {main= start_label; locals_types= R.empty_var_env} in
      let tail = explicate_control_tail e in
      Program (info, String.Map.singleton start_label tail)
      |> type_check_cvar

and explicate_control_tail = function
  | R_anf.(Atom a) -> Return (Atom a)
  | R_anf.(Prim p) -> Return (Prim p)
  | R_anf.(Let (v, e1, e2)) -> (
    match e1 with
    | R_anf.(Atom a) -> Seq (Assign (v, Atom a), explicate_control_tail e2)
    | R_anf.(Prim p) -> Seq (Assign (v, Prim p), explicate_control_tail e2)
    | _ -> failwith "C.explicate_control_tail: expected atom or prim" )

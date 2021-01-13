open Core_kernel

type label = string

type info = {main: label}

type var = R.var

type t = Program of info * tails

and tails = tail String.Map.t

and tail = Return of exp | Seq of stmt * tail

and stmt = Assign of var * exp

and exp = Atom of atom | Prim of prim

and atom = Int of int | Var of var

and prim = Read | Minus of atom | Plus of atom * atom

let rec to_string = function
  | Program (_, tails) ->
      let tls =
        Map.to_alist tails
        |> List.map ~f:(fun (l, t) ->
               Printf.sprintf "(%s:\n  %s)\n" l (string_of_tail t))
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

type env = exp String.Map.t

let empty_env = String.Map.empty

let read_int () =
  Out_channel.(flush stdout);
  Int.of_string In_channel.(input_line_exn stdin)

let rec interp ?(read = None) = function
  | Program (info, tails) -> (
    match Map.find tails info.main with
    | None -> failwith "C.interp: no main label defined"
    | Some t -> interp_tail empty_env t ~read )

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

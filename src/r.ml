open Core_kernel

type info = unit

type var = string

type t = Program of info * exp

and exp = Int of int | Prim of prim | Var of var | Let of var * exp * exp

and prim = Read | Minus of exp | Plus of exp * exp

let rec to_string = function
  | Program (_, exp) -> string_of_exp exp

and string_of_exp = function
  | Int i -> Int.to_string i
  | Prim p -> string_of_prim p
  | Var v -> v
  | Let (v, e1, e2) ->
      Printf.sprintf "(let ([%s %s]) %s)" v (string_of_exp e1)
        (string_of_exp e2)

and string_of_prim = function
  | Read -> "(read)"
  | Minus e -> Printf.sprintf "(- %s)" (string_of_exp e)
  | Plus (e1, e2) ->
      Printf.sprintf "(+ %s %s)" (string_of_exp e1) (string_of_exp e2)

let read_int () =
  Out_channel.(flush stdout);
  Int.of_string In_channel.(input_line_exn stdin)

type env = exp String.Map.t

let empty_env = String.Map.empty

let rec opt = function
  | Program (info, exp) -> Program (info, opt_exp empty_env exp)

and opt_exp env = function
  | Int _ as i -> i
  | Prim Read as r -> r
  | Prim (Minus e) -> (
    match opt_exp env e with
    | Int i -> Int (-i)
    | Prim (Minus e) -> e
    | x -> x )
  | Prim (Plus (e1, e2)) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int i1, Int i2 -> Int (i1 + i2)
    | Int i1, Prim (Minus (Int i2)) -> Int (i1 - i2)
    | Int i1, Prim (Plus (Int i2, e2)) | Prim (Plus (Int i1, e2)), Int i2 ->
        opt_exp env (Prim (Plus (Int (i1 + i2), e2)))
    | Prim (Minus (Int i1)), Int i2 -> Int (-i1 + i2)
    | e1, e2 -> Prim (Plus (e1, e2)) )
  | Var v -> Map.find_exn env v
  | Let (v, e1, e2) ->
      let e1 = opt_exp env e1 in
      opt_exp (Map.set env v e1) e2

let rec interp ?(read = None) = function
  | Program (_, exp) -> interp_exp empty_env exp ~read

and interp_exp ?(read = None) env = function
  | Int i -> i
  | Prim p -> interp_prim env p ~read
  | Var v -> Map.find_exn env v
  | Let (v, e1, e2) ->
      let e1 = interp_exp env e1 ~read in
      interp_exp (Map.set env v e1) e2 ~read

and interp_prim ?(read = None) env = function
  | Read -> (
    match read with
    | Some i -> i
    | None -> read_int () )
  | Minus e -> -interp_exp env e ~read
  | Plus (e1, e2) -> interp_exp env e1 ~read + interp_exp env e2 ~read

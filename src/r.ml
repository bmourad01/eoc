open Core_kernel

type info = unit

type t = Program of info * exp

and exp = Int of int | Prim of prim

and prim = Read | Minus of exp | Plus of exp * exp

let rec to_string = function
  | Program (_, exp) -> string_of_exp exp

and string_of_exp = function
  | Int i -> Int.to_string i
  | Prim p -> string_of_prim p

and string_of_prim = function
  | Read -> "(read)"
  | Minus e -> Printf.sprintf "(- %s)" (string_of_exp e)
  | Plus (e1, e2) ->
      Printf.sprintf "(+ %s %s)" (string_of_exp e1) (string_of_exp e2)

let read_int () =
  Out_channel.(flush stdout);
  Int.of_string In_channel.(input_line_exn stdin)

let rec opt = function
  | Program (info, exp) -> Program (info, opt_exp exp)

and opt_exp = function
  | Int _ as i -> i
  | Prim Read as r -> r
  | Prim (Minus e) -> (
    match opt_exp e with
    | Int i -> Int (-i)
    | Prim (Minus e) -> e
    | x -> x )
  | Prim (Plus (e1, e2)) -> (
    match (opt_exp e1, opt_exp e2) with
    | Int i1, Prim (Minus (Int i2)) -> Int (i1 - i2)
    | Int i1, Prim (Plus (Int i2, e2)) ->
        opt_exp (Prim (Plus (Int (i1 + i2), e2)))
    | Prim (Minus (Int i1)), Int i2 -> Int (-i1 + i2)
    | Prim (Plus (Int i1, e2)), Int i2 ->
        opt_exp (Prim (Plus (Int (i1 + i2), e2)))
    | e1, e2 -> Prim (Plus (e1, e2)) )

let rec interp ?(read = None) = function
  | Program (_, exp) -> interp_exp exp ~read

and interp_exp ?(read = None) = function
  | Int i -> i
  | Prim p -> interp_prim p ~read

and interp_prim ?(read = None) = function
  | Read -> (
    match read with
    | Some i -> i
    | None -> read_int () )
  | Minus e -> -interp_exp e ~read
  | Plus (e1, e2) -> interp_exp e1 ~read + interp_exp e2 ~read

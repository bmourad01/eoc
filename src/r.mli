open Core_kernel

type info = unit

type t = Program of info * exp

and exp = Int of int | Prim of prim

and prim = Read | Minus of exp | Plus of exp * exp

val to_string : t -> string

val string_of_exp : exp -> string

val string_of_prim : prim -> string

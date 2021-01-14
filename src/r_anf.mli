open Core_kernel

type info = R.info

type var = R.var

type t = Program of info * exp

and exp = Atom of atom | Prim of prim | Let of var * exp * exp

and atom = Int of int | Var of var

and prim = Read | Minus of atom | Plus of atom * atom

val to_string : t -> string

val string_of_exp : exp -> string

val string_of_atom : atom -> string

val string_of_prim : prim -> string

val resolve_complex : R.t -> t

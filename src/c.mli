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

val to_string : t -> string

val interp : ?read:int option -> t -> int

open Core_kernel

type label = string

type info = {main: label}

type var = R.var

(* the C intermediate language *)

type t = Program of info * tails

and tails = tail String.Map.t

and tail = Return of exp | Seq of stmt * tail

and stmt = Assign of var * exp

and exp = Atom of atom | Prim of prim

and atom = R_anf.atom

and prim = R_anf.prim

val to_string : t -> string

(* interpret a C program *)

val interp : ?read:int option -> t -> int

(* compile an R_anf program to a C program *)

val explicate_control : R_anf.t -> t

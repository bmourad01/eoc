open Core_kernel

type info = unit

type var = string

(* the R language: a subset of Racket *)

type t = Program of info * exp

and exp = Int of int | Prim of prim | Var of var | Let of var * exp * exp

and prim = Read | Minus of exp | Plus of exp * exp

val to_string : t -> string

val string_of_exp : exp -> string

val string_of_prim : prim -> string

(* optimize an R program *)

val opt : t -> t

(* interpret an R program *)

val interp : ?read:int option -> t -> int

(* make all let-bindings unique *)

val uniquify : t -> t

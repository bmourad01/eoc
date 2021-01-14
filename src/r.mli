open Core_kernel

type info = unit

type var = string [@@deriving equal, compare, sexp]

type 'a var_env = 'a String.Map.t

val empty_var_env : 'a var_env

module Type : sig
  type t = Integer

  val to_string : t -> string
end

type type_env = Type.t var_env

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

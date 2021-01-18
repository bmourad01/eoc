open Core_kernel

type var = string [@@deriving equal, compare, hash, sexp]

type info = unit

(* the R language: an untyped subset of Racket *)

type t = Program of info * exp

and exp =
  | Int of int
  | Bool of bool
  | Void
  | Prim of prim
  | Var of var
  | Let of var * exp * exp
  | If of exp * exp * exp

and prim =
  | Read
  | Minus of exp
  | Plus of exp * exp
  | Subtract of exp * exp
  | Mult of exp * exp
  | Div of exp * exp
  | Rem of exp * exp
  | Land of exp * exp
  | Lor of exp * exp
  | Lxor of exp * exp
  | Lnot of exp
  | Eq of exp * exp
  | Lt of exp * exp
  | Le of exp * exp
  | Gt of exp * exp
  | Ge of exp * exp
  | Not of exp
  | And of exp * exp
  | Or of exp * exp
  | Vector of exp list
  | Vectorlength of exp
  | Vectorref of exp * int
  | Vectorset of exp * int * exp

val to_string : t -> string

val string_of_exp : exp -> string

val string_of_prim : prim -> string

open Core_kernel

type var = R.var

module Type : module type of R.Type

type info = {typ: Type.t; nvars: int}

(* same as the R language, but with special
 * operators for allocating objects. *)

type t = Program of info * exp

and exp =
  | Int of int
  | Bool of bool
  | Void
  | Prim of prim
  | Var of var
  | Let of var * exp * exp
  | If of exp * exp * exp
  | Hastype of exp * Type.t
  | Collect of int
  | Allocate of int * Type.t
  | Globalvalue of string

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
  | Vectorlength of exp
  | Vectorref of exp * int
  | Vectorset of exp * int * exp

val to_string : ?has_type:bool -> t -> string

(* expand vector creation into calls to collect/allocate,
 * then initialize elements of the vector with vector-set! *)

val expose_allocation : R.t -> t

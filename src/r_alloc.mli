open Core_kernel

type var = R_typed.var

module Type : module type of R_typed.Type

type type_env = R_typed.type_env

type info = {typ: Type.t; nvars: int}

val free_ptr : string

val fromspace_end : string

(* same as the R_typed language, but with
 * special operators for allocating objects. *)

type t = Program of info * exp

and exp =
  | Int of int
  | Bool of bool
  | Void
  | Prim of prim * Type.t
  | Var of var * Type.t
  | Let of var * exp * exp * Type.t
  | If of exp * exp * exp * Type.t
  | Collect of int
  | Allocate of int * Type.t
  | Globalvalue of string * Type.t

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

val to_string : t -> string

(* expand vector creation into calls to collect/allocate,
 * then initialize elements of the vector with vector-set! *)

val expose_allocation : R_typed.t -> t

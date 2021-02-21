open Core_kernel

type var = R.var

module Type : module type of R_typed.Type

module Type_map : module type of R_typed.Type_map

type type_env = R_typed.type_env

type info = {nvars: int Label.Map.t}

val free_ptr : string

val fromspace_end : string

val word_size : int

val total_tag_offset : int

(* same as the R_typed language, but with
 * special operators for allocating objects. *)

type t = Program of info * def list

and def = Def of var * (var * Type.t) list * Type.t * exp

and exp =
  | Int of Int64.t
  | Float of float
  | Bool of bool
  | Void
  | Prim of prim * Type.t
  | Var of var * Type.t
  | Let of var * exp * exp * Type.t
  | If of exp * exp * exp * Type.t
  | Apply of exp * exp list * Type.t
  | Funref of var * Type.t
  | Setbang of var * exp
  | Begin of exp list * exp * Type.t
  | While of exp * exp
  | Collect of int
  | Allocate of int * Type.t
  | Globalvalue of string * Type.t

and prim =
  | Read
  | Print of exp
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
  | Neq of exp * exp
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

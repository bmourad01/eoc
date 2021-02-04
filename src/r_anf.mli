open Core_kernel

type var = R_alloc.var

module Type : module type of R_alloc.Type

module Type_map : module type of R_alloc.Type_map

type type_env = R_alloc.type_env

type info = {nvars: int Label.Map.t}

(* the R language in A-normal form,
 * equivalent to continuation-passing style (CPS).
 *
 * we use a separate module for this language,
 * instead of just using R, because the type
 * system will guarantee that the program
 * has the ANF structure. *)

type t = Program of info * def list

and def = Def of var * (var * Type.t) list * Type.t * exp

and exp =
  | Atom of atom
  | Prim of prim * Type.t
  | Let of var * exp * exp * Type.t
  | If of exp * exp * exp * Type.t
  | Apply of atom * atom list * Type.t
  | Funref of var * Type.t
  | Setbang of var * exp
  | Begin of exp list * exp * Type.t
  | While of exp * exp
  | Collect of int
  | Allocate of int * Type.t
  | Globalvalue of string * Type.t

and atom = Int of Int64.t | Bool of bool | Var of var * Type.t | Void

and prim =
  | Read
  | Print of atom
  | Minus of atom
  | Plus of atom * atom
  | Subtract of atom * atom
  | Mult of atom * atom
  | Div of atom * atom
  | Rem of atom * atom
  | Land of atom * atom
  | Lor of atom * atom
  | Lxor of atom * atom
  | Lnot of atom
  | Eq of atom * atom
  | Neq of atom * atom
  | Lt of atom * atom
  | Le of atom * atom
  | Gt of atom * atom
  | Ge of atom * atom
  | Not of atom
  | Vectorlength of atom
  | Vectorref of atom * int
  | Vectorset of atom * int * atom

val typeof_exp : exp -> Type.t

val to_string : t -> string

val string_of_exp : exp -> string

val string_of_atom : atom -> string

val string_of_prim : prim -> string

(* compile an R_alloc program to an R_anf program *)

val resolve_complex : R_alloc.t -> t

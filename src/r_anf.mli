open Core_kernel

type var = R.var

module Type : module type of R_alloc.Type

type info = {typ: Type.t}

(* the R language in A-normal form,
 * equivalent to continuation-passing style (CPS).
 *
 * we use a separate module for this language,
 * instead of just using R, because the type
 * system will guarantee that the program
 * has the ANF structure. *)

type t = Program of info * exp

and exp =
  | Atom of atom
  | Prim of prim
  | Let of var * exp * exp
  | If of exp * exp * exp
  | Collect of int
  | Allocate of int * Type.t
  | Globalvalue of string

and atom =
  | Int of int
  | Bool of bool
  | Var of var
  | Void
  | Hastype of exp * Type.t

and prim =
  | Read
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
  | Lt of atom * atom
  | Le of atom * atom
  | Gt of atom * atom
  | Ge of atom * atom
  | Not of atom
  | Vectorlength of atom
  | Vectorref of atom * int
  | Vectorset of atom * int * atom

val to_string : ?has_type:bool -> t -> string

val string_of_exp : ?has_type:bool -> exp -> string

val string_of_atom : ?has_type:bool -> atom -> string

val string_of_prim : ?has_type:bool -> prim -> string

(* compile an R_alloc program to an R_anf program *)

val resolve_complex : R_alloc.t -> t

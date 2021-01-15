open Core_kernel

type info = R.info

type var = R.var

(* the R language in A-normal form,
 * equivalent to continuation-passing style (CPS).
 *
 * we use a separate module for this language,
 * instead of just using R, because the type
 * system will guarantee that the program
 * has the ANF structure. *)

type t = Program of info * exp

and exp = Atom of atom | Prim of prim | Let of var * exp * exp

and atom = Int of int | Var of var

and prim =
  | Read
  | Minus of atom
  | Plus of atom * atom
  | Subtract of atom * atom

val to_string : t -> string

val string_of_exp : exp -> string

val string_of_atom : atom -> string

val string_of_prim : prim -> string

(* compile an R program to an R_anf program *)

val resolve_complex : R.t -> t

open Core_kernel

module Type : module type of R_anf.Type

type type_env = R_anf.type_env

module Cfg : module type of Graph.Persistent.Digraph.Concrete (Label)

type info = {main: Label.t; typ: Type.t; cfg: Cfg.t}

type var = R.var

module Cmp : sig
  type t = Eq | Lt | Le | Gt | Ge

  val to_string : t -> string
end

(* the C intermediate language *)

type t = Program of info * tails

and tails = (Label.t * tail) list

and tail =
  | Return of exp
  | Seq of stmt * tail
  | Goto of Label.t
  | If of cmp * Label.t * Label.t

and stmt = Assign of var * exp | Collect of int

and exp =
  | Atom of atom
  | Prim of prim * Type.t
  | Allocate of int * Type.t
  | Globalvalue of string * Type.t

and atom = Int of int | Bool of bool | Var of var * Type.t | Void

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

and cmp = Cmp.t * atom * atom

val to_string : t -> string

val string_of_tail : tail -> string

val string_of_stmt : stmt -> string

val string_of_exp : exp -> string

val string_of_atom : atom -> string

val string_of_prim : prim -> string

val string_of_cmp : cmp -> string

(* interpret a C program *)

type answer = R_typed.answer

val interp : ?read:int option -> t -> answer

(* compile an R_anf program to a C program *)

val explicate_control : R_anf.t -> t

(* collapse sequences of jumps through trivial blocks *)

val optimize_jumps : t -> t

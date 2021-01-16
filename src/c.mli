open Core_kernel

module Type : module type of R.Type

type type_env = R.type_env

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

and stmt = Assign of var * exp

and exp = Atom of atom | Prim of prim

and atom = R_anf.atom

and prim = R_anf.prim

and cmp = Cmp.t * atom * atom

val to_string : t -> string

val string_of_tail : tail -> string

val string_of_stmt : stmt -> string

val string_of_exp : exp -> string

val string_of_atom : atom -> string

val string_of_prim : prim -> string

val string_of_cmp : cmp -> string

(* interpret a C program *)

type answer = R.answer

val interp : ?read:int option -> t -> answer

(* compile an R_anf program to a C program *)

val explicate_control : R_anf.t -> t

(* collapse sequences of jumps through trivial blocks *)

val optimize_jumps : t -> t

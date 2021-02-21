open Core_kernel

module Type : module type of R_anf.Type

module Type_map : module type of R_anf.Type_map

type type_env = R_anf.type_env

type var = R.var

type info = {main: Label.t}

module Cmp : sig
  type t = Eq | Neq | Lt | Le | Gt | Ge

  val to_string : t -> string
end

module Cfg : module type of Cfg.Make (Label)

(* the C intermediate language *)

type t = Program of info * def list

and def = Def of Label.t * (var * Type.t) list * Type.t * def_info * tails

and def_info = {main: Label.t; cfg: Cfg.t; locals_types: type_env}

and tails = (Label.t * tail) list

and tail =
  | Return of exp
  | Seq of stmt * tail
  | Goto of Label.t
  | If of cmp * Label.t * Label.t
  | Tailcall of atom * atom list * Type.t

and stmt =
  | Assign of var * exp
  | Collect of int
  | Callstmt of atom * atom list
  | Vectorsetstmt of atom * int * atom
  | Readstmt
  | Printstmt of atom
  | Assignwhen of cmp * var * atom

and exp =
  | Atom of atom
  | Prim of prim * Type.t
  | Funref of Label.t * Type.t
  | Call of atom * atom list * Type.t
  | Allocate of int * Type.t
  | Globalvalue of string * Type.t
  | Select of cmp * atom * atom * Type.t

and atom =
  | Int of Int64.t
  | Float of float
  | Bool of bool
  | Var of var * Type.t
  | Void

and prim =
  | Read
  | Minus of atom
  | Sqrt of atom
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

and cmp = Cmp.t * atom * atom

val typeof_exp : exp -> Type.t

val to_string : t -> string

val string_of_tail : tail -> string

val string_of_stmt : stmt -> string

val string_of_exp : exp -> string

val string_of_atom : atom -> string

val string_of_prim : prim -> string

val string_of_cmp : cmp -> string

(* interpret a C program *)

type answer = R_typed.answer

val string_of_answer : ?nested:bool -> answer -> string

val interp : ?read:Int64.t option -> t -> answer

(* compile an R_anf program to a C program *)

val explicate_control : R_anf.t -> t

(* collapse sequences of jumps through trivial blocks *)

val optimize_jumps : t -> t

open Core_kernel

type var = R.var [@@deriving equal, compare, hash, sexp]

type 'a var_env = 'a String.Map.t

val empty_var_env : 'a var_env

module Type : sig
  type t = Integer | Boolean | Vector of t list | Void [@@deriving equal]

  val to_string : t -> string
end

type type_env = Type.t var_env

type info = {typ: Type.t}

(* a type-checked R program *)

type t = Program of info * exp

(* values like Int, Bool, Void always have the
 * same type, so we don't need to annotate them *)
and exp =
  | Int of int
  | Bool of bool
  | Void
  | Prim of prim * Type.t
  | Var of var * Type.t
  | Let of var * exp * exp * Type.t
  | If of exp * exp * exp * Type.t

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

(* optimize a program *)

val opt : t -> t

(* take an untyped R program and produce an R_typed program *)

exception Type_error of string

val type_check : R.t -> t

(* the result of evaluating a program *)

type answer = [`Int of int | `Bool of bool | `Vector of answer array | `Void]

val string_of_answer : ?nested:bool -> answer -> string

(* interpret a program *)

val interp : ?read:int option -> t -> answer

(* make all let-bound variables unique *)

val uniquify : t -> t

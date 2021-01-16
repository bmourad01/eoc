open Core_kernel

type var = string [@@deriving equal, compare, hash, sexp]

type 'a var_env = 'a String.Map.t

val empty_var_env : 'a var_env

module Type : sig
  type t = Integer | Boolean [@@deriving equal]

  val to_string : t -> string
end

type type_env = Type.t var_env

type info = {typ: Type.t}

(* the R language: a subset of Racket *)

type t = Program of info * exp

and exp =
  | Int of int
  | Bool of bool
  | Prim of prim
  | Var of var
  | Let of var * exp * exp
  | If of exp * exp * exp

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

val to_string : t -> string

val string_of_exp : exp -> string

val string_of_prim : prim -> string

val type_check : t -> Type.t

val type_check_exp : Type.t var_env -> exp -> Type.t

type answer = [`Int of int | `Bool of bool]

(* optimize an R program *)

val opt : t -> t

(* interpret an R program *)

val interp : ?read:int option -> t -> answer

(* make all let-bindings unique *)

val uniquify : t -> t

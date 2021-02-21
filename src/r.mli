open Core_kernel

type var = string [@@deriving equal, compare, hash, sexp]

module Type : sig
  type t =
    | Integer
    | Float
    | Boolean
    | Void
    | Vector of t list
    | Arrow of t list * t
    | Trustme
  [@@deriving equal, compare, sexp]

  val to_string : t -> string
end

module Type_map : module type of Map.Make (Type)

type info = unit

(* the R language: a subset of Typed Racket *)

type t = Program of info * def list * exp

and def = Def of var * (var * Type.t) list * Type.t * exp

and exp =
  | Int of Int64.t
  | Float of float
  | Bool of bool
  | Void
  | Prim of prim
  | Var of var
  | Let of var * exp * exp
  | Letm of (var * exp) list * exp
  | If of exp * exp * exp
  | Apply of exp * exp list
  | Lambda of (var * Type.t) list * Type.t * exp
  | Setbang of var * exp
  | Begin of exp list * exp
  | When of exp * exp list
  | Unless of exp * exp list
  | While of exp * exp

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
  | Procedurearity of exp

val to_string : t -> string

val string_of_exp : exp -> string

val string_of_prim : prim -> string

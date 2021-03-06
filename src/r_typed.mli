open Core_kernel

type var = R.var [@@deriving equal, compare, hash, sexp]

type 'a var_env = 'a String.Map.t

val empty_var_env : 'a var_env

module Type : module type of R.Type

module Type_map : module type of R.Type_map

val main : Label.t

type type_env = Type.t var_env

type info = unit

(* a type-checked R program.
 *
 * we deviate from the language described in the book
 * by embedding the types of expressions in the AST itself. *)

type t = Program of info * def list

and def = Def of var * (var * Type.t) list * Type.t * exp

(* values like Int, Bool, Void always have the
 * same type, so we don't need to annotate them *)
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
  | Lambda of (var * Type.t) list * Type.t * exp
  | Setbang of var * exp
  | Begin of exp list * exp * Type.t
  | While of exp * exp

and prim =
  | Read
  | Print of exp
  | Minus of exp
  | Sqrt of exp
  | Int2float of exp
  | Float2int of exp
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

type answer =
  [ `Int of Int64.t
  | `Float of float
  | `Bool of bool
  | `Void
  | `Vector of answer array
  | `Def of var
  | `Function of answer var_env * var list * exp ]

val string_of_answer : ?nested:bool -> answer -> string

(* interpret a program *)

val interp : ?read:Int64.t option -> t -> answer

(* make all let-bound variables unique *)

val uniquify : t -> t

(* box mutable variables that escape into closures *)

val convert_assignments : t -> t

(* compile lambdas to top-level functions *)

val convert_to_closures : t -> t

(* limit arity of functions + calls *)

val limit_functions : t -> t

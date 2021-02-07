open Core_kernel

type var = string [@@deriving equal, compare, hash, sexp]

module Type = struct
  module T = struct
    type t =
      | Integer
      | Boolean
      | Void
      | Vector of t list
      | Arrow of t list * t
      | Trustme
    [@@deriving equal, compare, sexp]

    let rec to_string = function
      | Integer -> "Integer"
      | Boolean -> "Boolean"
      | Void -> "Void"
      | Vector ts ->
          let s = List.map ts ~f:to_string in
          if List.is_empty s then "(Vector)"
          else Printf.sprintf "(Vector %s)" (String.concat s ~sep:" ")
      | Arrow ([], t) -> Printf.sprintf "(-> %s)" (to_string t)
      | Arrow (ts, t) ->
          Printf.sprintf "(-> %s %s)"
            (List.map ts ~f:to_string |> String.concat ~sep:" ")
            (to_string t)
      | Trustme -> "_"
  end

  include T
  include Comparable.Make (T)
end

module Type_map = Map.Make (Type)

type info = unit

type t = Program of info * def list * exp

and def = Def of var * (var * Type.t) list * Type.t * exp

and exp =
  | Int of Int64.t
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

let rec to_string = function
  | Program (_, defs, exp) ->
      let ds = List.map defs ~f:string_of_def |> String.concat ~sep:"\n\n" in
      if String.is_empty ds then string_of_exp exp
      else ds ^ "\n\n" ^ string_of_exp exp

and string_of_def = function
  | Def (v, args, t, e) ->
      let s =
        List.map args ~f:(fun (a, t) ->
            Printf.sprintf "[%s : %s]" a (Type.to_string t))
        |> String.concat ~sep:" "
      in
      if String.is_empty s then
        Printf.sprintf "(define (%s) : %s %s)" v (Type.to_string t)
          (string_of_exp e)
      else
        Printf.sprintf "(define (%s %s) : %s %s)" v s (Type.to_string t)
          (string_of_exp e)

and string_of_exp = function
  | Int i -> Int64.to_string i
  | Bool b -> if b then "#t" else "#f"
  | Void -> "(void)"
  | Prim p -> string_of_prim p
  | Var v -> v
  | Let (v, e1, e2) ->
      Printf.sprintf "(let ([%s %s]) %s)" v (string_of_exp e1)
        (string_of_exp e2)
  | Letm (bnd, e) ->
      Printf.sprintf "(let (%s) %s)"
        ( List.map bnd ~f:(fun (x, e) ->
              Printf.sprintf "[%s %s]" x (string_of_exp e))
        |> String.concat ~sep:" " )
        (string_of_exp e)
  | If (e1, e2, e3) ->
      Printf.sprintf "(if %s %s %s)" (string_of_exp e1) (string_of_exp e2)
        (string_of_exp e3)
  | Apply (e, []) -> Printf.sprintf "(%s)" (string_of_exp e)
  | Apply (e, es) ->
      Printf.sprintf "(%s %s)" (string_of_exp e)
        (List.map es ~f:string_of_exp |> String.concat ~sep:" ")
  | Lambda (args, t, e) ->
      let s =
        List.map args ~f:(fun (a, t) ->
            Printf.sprintf "[%s : %s]" a (Type.to_string t))
        |> String.concat ~sep:" "
      in
      Printf.sprintf "(lambda: (%s) : %s %s)" s (Type.to_string t)
        (string_of_exp e)
  | Setbang (v, e) -> Printf.sprintf "(set! %s %s)" v (string_of_exp e)
  | Begin ([], e) -> Printf.sprintf "(begin %s)" (string_of_exp e)
  | Begin (es, e) ->
      Printf.sprintf "(begin %s %s)"
        (List.map es ~f:string_of_exp |> String.concat ~sep:" ")
        (string_of_exp e)
  | When (e, es) ->
      Printf.sprintf "(when %s %s)" (string_of_exp e)
        (List.map es ~f:string_of_exp |> String.concat ~sep:" ")
  | Unless (e, es) ->
      Printf.sprintf "(unless %s %s)" (string_of_exp e)
        (List.map es ~f:string_of_exp |> String.concat ~sep:" ")
  | While (e1, e2) ->
      Printf.sprintf "(while %s %s)" (string_of_exp e1) (string_of_exp e2)

and string_of_prim = function
  | Read -> "(read)"
  | Print e -> Printf.sprintf "(print %s)" (string_of_exp e)
  | Minus e -> Printf.sprintf "(- %s)" (string_of_exp e)
  | Plus (e1, e2) ->
      Printf.sprintf "(+ %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Subtract (e1, e2) ->
      Printf.sprintf "(- %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Mult (e1, e2) ->
      Printf.sprintf "(* %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Div (e1, e2) ->
      Printf.sprintf "(/ %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Rem (e1, e2) ->
      Printf.sprintf "(rem %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Land (e1, e2) ->
      Printf.sprintf "(land %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Lor (e1, e2) ->
      Printf.sprintf "(lor %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Lxor (e1, e2) ->
      Printf.sprintf "(lxor %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Lnot e -> Printf.sprintf "(lnot %s)" (string_of_exp e)
  | Eq (e1, e2) ->
      Printf.sprintf "(eq? %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Lt (e1, e2) ->
      Printf.sprintf "(< %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Le (e1, e2) ->
      Printf.sprintf "(<= %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Gt (e1, e2) ->
      Printf.sprintf "(> %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Ge (e1, e2) ->
      Printf.sprintf "(>= %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Not e -> Printf.sprintf "(not %s)" (string_of_exp e)
  | And (e1, e2) ->
      Printf.sprintf "(and %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Or (e1, e2) ->
      Printf.sprintf "(or %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Vector es ->
      let s = List.map es ~f:string_of_exp in
      if List.is_empty s then "(vector)"
      else Printf.sprintf "(vector %s)" (String.concat s ~sep:" ")
  | Vectorlength e -> Printf.sprintf "(vector-length %s)" (string_of_exp e)
  | Vectorref (e, i) ->
      Printf.sprintf "(vector-ref %s %d)" (string_of_exp e) i
  | Vectorset (e1, i, e2) ->
      Printf.sprintf "(vector-set! %s %d %s)" (string_of_exp e1) i
        (string_of_exp e2)
  | Procedurearity e ->
      Printf.sprintf "(procedure-arity %s)" (string_of_exp e)

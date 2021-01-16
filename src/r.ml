open Core_kernel

type var = string [@@deriving equal, compare, hash, sexp]

type 'a var_env = 'a String.Map.t

let empty_var_env = String.Map.empty

module Type = struct
  type t = Integer | Boolean [@@deriving equal]

  let to_string = function
    | Integer -> "Integer"
    | Boolean -> "Boolean"
end

type type_env = Type.t var_env

type info = {typ: Type.t} [@@deriving equal]

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
[@@deriving equal]

let rec to_string = function
  | Program (_, exp) -> string_of_exp exp

and string_of_exp = function
  | Int i -> Int.to_string i
  | Bool b -> if b then "#t" else "#f"
  | Prim p -> string_of_prim p
  | Var v -> v
  | Let (v, e1, e2) ->
      Printf.sprintf "(let ([%s %s]) %s)" v (string_of_exp e1)
        (string_of_exp e2)
  | If (e1, e2, e3) ->
      Printf.sprintf "(if %s %s %s)" (string_of_exp e1) (string_of_exp e2)
        (string_of_exp e3)

and string_of_prim = function
  | Read -> "(read)"
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

let read_int () =
  Out_channel.(flush stdout);
  Int.of_string In_channel.(input_line_exn stdin)

let rec opt = function
  | Program (info, exp) -> Program (info, opt_exp empty_var_env exp)

and opt_exp env = function
  | Int _ as i -> i
  | Bool _ as b -> b
  | Prim Read as r -> r
  | Prim (Minus e) -> (
    match opt_exp env e with
    | Int i -> Int (-i)
    | Prim (Minus e) -> e
    | x -> x )
  | Prim (Plus (e1, e2)) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int i1, Int i2 -> Int (i1 + i2)
    | Int i1, Prim (Minus (Int i2)) -> Int (i1 - i2)
    | Int i1, Prim (Plus (Int i2, e2)) | Prim (Plus (Int i1, e2)), Int i2 ->
        opt_exp env (Prim (Plus (Int (i1 + i2), e2)))
    | Prim (Minus (Int i1)), Int i2 -> Int (-i1 + i2)
    | e1, e2 -> Prim (Plus (e1, e2)) )
  | Prim (Subtract (e1, e2)) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int i1, Int i2 -> Int (i1 - i2)
    | Int i1, Prim (Minus (Int i2)) -> Int (i1 + i2)
    | Prim (Minus (Int i1)), Int i2 -> Int (-i1 - i2)
    | e1, e2 -> Prim (Subtract (e1, e2)) )
  | Prim (Mult (e1, e2)) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int 0, _ -> Int 0
    | _, Int 0 -> Int 0
    | Int i1, Int i2 -> Int (i1 * i2)
    | e1, e2 -> Prim (Mult (e1, e2)) )
  | Prim (Div (e1, e2)) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int 0, _ -> Int 0
    | _, Int 0 -> failwith "R.opt_exp: divide by zero"
    | Int i1, Int i2 -> Int (i1 / i2)
    | e1, e2 -> Prim (Div (e1, e2)) )
  | Prim (Rem (e1, e2)) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int 0, _ -> Int 0
    | _, Int 0 -> failwith "R.opt_exp: divide by zero"
    | Int i1, Int i2 -> Int (i1 mod i2)
    | e1, e2 -> Prim (Rem (e1, e2)) )
  | Prim (Land (e1, e2)) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int 0, _ -> Int 0
    | _, Int 0 -> Int 0
    | Int i1, Int i2 -> Int (i1 land i2)
    | e1, e2 -> Prim (Land (e1, e2)) )
  | Prim (Lor (e1, e2)) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int 0, e -> e
    | e, Int 0 -> Int 0
    | Int i1, Int i2 -> Int (i1 lor i2)
    | e1, e2 -> Prim (Lor (e1, e2)) )
  | Prim (Lxor (e1, e2)) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int i1, Int i2 -> Int (i1 lxor i2)
    | e1, e2 -> Prim (Lxor (e1, e2)) )
  | Prim (Lnot e) -> (
    match opt_exp env e with
    | Int i -> Int (lnot i)
    | e -> Prim (Lnot e) )
  | Prim (Eq (e1, e2)) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int i1, Int i2 -> Bool (i1 = i2)
    | Bool b1, Bool b2 -> Bool (Bool.equal b1 b2)
    | e1, e2 -> Prim (Eq (e1, e2)) )
  | Prim (Lt (e1, e2)) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int i1, Int i2 -> Bool (i1 < i2)
    | e1, e2 -> Prim (Lt (e1, e2)) )
  | Prim (Le (e1, e2)) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int i1, Int i2 -> Bool (i1 <= i2)
    | e1, e2 -> Prim (Le (e1, e2)) )
  | Prim (Gt (e1, e2)) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int i1, Int i2 -> Bool (i1 > i2)
    | e1, e2 -> Prim (Gt (e1, e2)) )
  | Prim (Ge (e1, e2)) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Int i1, Int i2 -> Bool (i1 >= i2)
    | e1, e2 -> Prim (Ge (e1, e2)) )
  | Prim (Not e) -> (
    match opt_exp env e with
    | Bool b -> Bool (not b)
    | e -> Prim (Not e) )
  | Prim (And (e1, e2)) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Bool b1, Bool b2 -> Bool (b1 && b2)
    | e1, e2 -> Prim (And (e1, e2)) )
  | Prim (Or (e1, e2)) -> (
    match (opt_exp env e1, opt_exp env e2) with
    | Bool b1, Bool b2 -> Bool (b1 || b2)
    | e1, e2 -> Prim (Or (e1, e2)) )
  | Var v -> (
    match Map.find env v with
    | None -> failwith ("R.opt_exp: var " ^ v ^ " is not bound")
    | Some e -> e )
  | Let (v, Prim Read, _) as e -> e
  | Let (v, e1, e2) ->
      let e1 = opt_exp env e1 in
      let e2 = opt_exp (Map.set env v e1) e2 in
      e2
  | If (e1, e2, e3) -> (
    match opt_exp env e1 with
    | Bool true -> opt_exp env e2
    | Bool false -> opt_exp env e3
    | e1 -> If (e1, e2, e3) )

let rec type_check = function
  | Program (_, exp) -> type_check_exp empty_var_env exp

and type_check_exp env = function
  | Int _ -> Type.Integer
  | Bool _ -> Type.Boolean
  | Prim p -> type_check_prim env p
  | Var v -> (
    match Map.find env v with
    | None -> failwith ("R.type_check_exp: var " ^ v ^ " is not bound")
    | Some typ -> typ )
  | Let (v, e1, e2) ->
      let t1 = type_check_exp env e1 in
      let env = Map.set env v t1 in
      type_check_exp env e2
  | If (e1, e2, e3) -> (
    match type_check_exp env e1 with
    | Type.Boolean ->
        let t1 = type_check_exp env e2 in
        let t2 = type_check_exp env e3 in
        if Type.equal t1 t2 then t1
        else
          failwith
            ( "R.type_check_exp: if branches " ^ string_of_exp e2 ^ " ("
            ^ Type.to_string t1 ^ ") and " ^ string_of_exp e3 ^ " ("
            ^ Type.to_string t2 ^ ") are not the same type" )
    | t ->
        failwith
          ( "R.type_check_exp: if condition " ^ string_of_exp e1
          ^ " is of type " ^ Type.to_string t
          ^ " but an expression of type Boolean was expected" ) )

and type_check_prim env = function
  | Read -> Type.Integer
  | Minus e -> (
    match type_check_exp env e with
    | Type.Integer -> Type.Integer
    | t ->
        failwith
          ( "R.type_check_prim: minus exp " ^ string_of_exp e ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" ) )
  | Plus (e1, e2) -> (
    match (type_check_exp env e1, type_check_exp env e2) with
    | Type.Integer, Type.Integer -> Type.Integer
    | t, Type.Integer ->
        failwith
          ( "R.type_check_prim: plus exp " ^ string_of_exp e1 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | Type.Integer, t ->
        failwith
          ( "R.type_check_prim: plus exp " ^ string_of_exp e2 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | t1, t2 ->
        failwith
          ( "R.type_check_prim: plus exps " ^ string_of_exp e1 ^ " and "
          ^ string_of_exp e2 ^ " have types " ^ Type.to_string t1 ^ " and "
          ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | Subtract (e1, e2) -> (
    match (type_check_exp env e1, type_check_exp env e2) with
    | Type.Integer, Type.Integer -> Type.Integer
    | t, Type.Integer ->
        failwith
          ( "R.type_check_prim: subtract exp " ^ string_of_exp e1
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | Type.Integer, t ->
        failwith
          ( "R.type_check_prim: subtract exp " ^ string_of_exp e2
          ^ " has type " ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | t1, t2 ->
        failwith
          ( "R.type_check_prim: subtract exps " ^ string_of_exp e1 ^ " and "
          ^ string_of_exp e2 ^ " have types " ^ Type.to_string t1 ^ " and "
          ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | Mult (e1, e2) -> (
    match (type_check_exp env e1, type_check_exp env e2) with
    | Type.Integer, Type.Integer -> Type.Integer
    | t, Type.Integer ->
        failwith
          ( "R.type_check_prim: mult exp " ^ string_of_exp e1 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | Type.Integer, t ->
        failwith
          ( "R.type_check_prim: mult exp " ^ string_of_exp e2 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | t1, t2 ->
        failwith
          ( "R.type_check_prim: mult exps " ^ string_of_exp e1 ^ " and "
          ^ string_of_exp e2 ^ " have types " ^ Type.to_string t1 ^ " and "
          ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | Div (e1, e2) -> (
    match (type_check_exp env e1, type_check_exp env e2) with
    | Type.Integer, Type.Integer -> Type.Integer
    | t, Type.Integer ->
        failwith
          ( "R.type_check_prim: div exp " ^ string_of_exp e1 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | Type.Integer, t ->
        failwith
          ( "R.type_check_prim: div exp " ^ string_of_exp e2 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | t1, t2 ->
        failwith
          ( "R.type_check_prim: div exps " ^ string_of_exp e1 ^ " and "
          ^ string_of_exp e2 ^ " have types " ^ Type.to_string t1 ^ " and "
          ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | Rem (e1, e2) -> (
    match (type_check_exp env e1, type_check_exp env e2) with
    | Type.Integer, Type.Integer -> Type.Integer
    | t, Type.Integer ->
        failwith
          ( "R.type_check_prim: rem exp " ^ string_of_exp e1 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | Type.Integer, t ->
        failwith
          ( "R.type_check_prim: rem exp " ^ string_of_exp e2 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | t1, t2 ->
        failwith
          ( "R.type_check_prim: rem exps " ^ string_of_exp e1 ^ " and "
          ^ string_of_exp e2 ^ " have types " ^ Type.to_string t1 ^ " and "
          ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | Land (e1, e2) -> (
    match (type_check_exp env e1, type_check_exp env e2) with
    | Type.Integer, Type.Integer -> Type.Integer
    | t, Type.Integer ->
        failwith
          ( "R.type_check_prim: land exp " ^ string_of_exp e1 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | Type.Integer, t ->
        failwith
          ( "R.type_check_prim: land exp " ^ string_of_exp e2 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | t1, t2 ->
        failwith
          ( "R.type_check_prim: land exps " ^ string_of_exp e1 ^ " and "
          ^ string_of_exp e2 ^ " have types " ^ Type.to_string t1 ^ " and "
          ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | Lor (e1, e2) -> (
    match (type_check_exp env e1, type_check_exp env e2) with
    | Type.Integer, Type.Integer -> Type.Integer
    | t, Type.Integer ->
        failwith
          ( "R.type_check_prim: lor exp " ^ string_of_exp e1 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | Type.Integer, t ->
        failwith
          ( "R.type_check_prim: lor exp " ^ string_of_exp e2 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | t1, t2 ->
        failwith
          ( "R.type_check_prim: lor exps " ^ string_of_exp e1 ^ " and "
          ^ string_of_exp e2 ^ " have types " ^ Type.to_string t1 ^ " and "
          ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | Lxor (e1, e2) -> (
    match (type_check_exp env e1, type_check_exp env e2) with
    | Type.Integer, Type.Integer -> Type.Integer
    | t, Type.Integer ->
        failwith
          ( "R.type_check_prim: lxor exp " ^ string_of_exp e1 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | Type.Integer, t ->
        failwith
          ( "R.type_check_prim: lxor exp " ^ string_of_exp e2 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | t1, t2 ->
        failwith
          ( "R.type_check_prim: lxor exps " ^ string_of_exp e1 ^ " and "
          ^ string_of_exp e2 ^ " have types " ^ Type.to_string t1 ^ " and "
          ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | Lnot e -> (
    match type_check_exp env e with
    | Type.Integer -> Type.Integer
    | t ->
        failwith
          ( "R.type_check_prim: lnot exp " ^ string_of_exp e ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" ) )
  | Eq (e1, e2) ->
      let t1 = type_check_exp env e1 in
      let t2 = type_check_exp env e2 in
      if Type.equal t1 t2 then Type.Boolean
      else
        failwith
          ( "R.type_check_prim: eq? exps " ^ string_of_exp e1 ^ " ("
          ^ Type.to_string t1 ^ ") and " ^ string_of_exp e2 ^ " ("
          ^ Type.to_string t2 ^ ") are not the same type" )
  | Lt (e1, e2) -> (
    match (type_check_exp env e1, type_check_exp env e2) with
    | Type.Integer, Type.Integer -> Type.Boolean
    | t, Type.Integer ->
        failwith
          ( "R.type_check_prim: < exp " ^ string_of_exp e1 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | Type.Integer, t ->
        failwith
          ( "R.type_check_prim: < exp " ^ string_of_exp e2 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | t1, t2 ->
        failwith
          ( "R.type_check_prim: < exps " ^ string_of_exp e1 ^ " and "
          ^ string_of_exp e2 ^ " have types " ^ Type.to_string t1 ^ " and "
          ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | Le (e1, e2) -> (
    match (type_check_exp env e1, type_check_exp env e2) with
    | Type.Integer, Type.Integer -> Type.Boolean
    | t, Type.Integer ->
        failwith
          ( "R.type_check_prim: <= exp " ^ string_of_exp e1 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | Type.Integer, t ->
        failwith
          ( "R.type_check_prim: <= exp " ^ string_of_exp e2 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | t1, t2 ->
        failwith
          ( "R.type_check_prim: <= exps " ^ string_of_exp e1 ^ " and "
          ^ string_of_exp e2 ^ " have types " ^ Type.to_string t1 ^ " and "
          ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | Gt (e1, e2) -> (
    match (type_check_exp env e1, type_check_exp env e2) with
    | Type.Integer, Type.Integer -> Type.Boolean
    | t, Type.Integer ->
        failwith
          ( "R.type_check_prim: > exp " ^ string_of_exp e1 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | Type.Integer, t ->
        failwith
          ( "R.type_check_prim: > exp " ^ string_of_exp e2 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | t1, t2 ->
        failwith
          ( "R.type_check_prim: > exps " ^ string_of_exp e1 ^ " and "
          ^ string_of_exp e2 ^ " have types " ^ Type.to_string t1 ^ " and "
          ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | Ge (e1, e2) -> (
    match (type_check_exp env e1, type_check_exp env e2) with
    | Type.Integer, Type.Integer -> Type.Boolean
    | t, Type.Integer ->
        failwith
          ( "R.type_check_prim: >= exp " ^ string_of_exp e1 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | Type.Integer, t ->
        failwith
          ( "R.type_check_prim: >= exp " ^ string_of_exp e2 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Integer was expected" )
    | t1, t2 ->
        failwith
          ( "R.type_check_prim: >= exps " ^ string_of_exp e1 ^ " and "
          ^ string_of_exp e2 ^ " have types " ^ Type.to_string t1 ^ " and "
          ^ Type.to_string t2
          ^ " but expressions of type Integer were expected" ) )
  | Not e -> (
    match type_check_exp env e with
    | Type.Boolean -> Type.Boolean
    | t ->
        failwith
          ( "R.type_check_prim: minus exp " ^ string_of_exp e ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Boolean was expected" ) )
  | And (e1, e2) -> (
    match (type_check_exp env e1, type_check_exp env e2) with
    | Type.Boolean, Type.Boolean -> Type.Boolean
    | t, Type.Boolean ->
        failwith
          ( "R.type_check_prim: and exp " ^ string_of_exp e1 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Boolean was expected" )
    | Type.Boolean, t ->
        failwith
          ( "R.type_check_prim: and exp " ^ string_of_exp e2 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Boolean was expected" )
    | t1, t2 ->
        failwith
          ( "R.type_check_prim: and exps " ^ string_of_exp e1 ^ " and "
          ^ string_of_exp e2 ^ " have types " ^ Type.to_string t1 ^ " and "
          ^ Type.to_string t2
          ^ " but expressions of type Boolean were expected" ) )
  | Or (e1, e2) -> (
    match (type_check_exp env e1, type_check_exp env e2) with
    | Type.Boolean, Type.Boolean -> Type.Boolean
    | t, Type.Boolean ->
        failwith
          ( "R.type_check_prim: or exp " ^ string_of_exp e1 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Boolean was expected" )
    | Type.Boolean, t ->
        failwith
          ( "R.type_check_prim: or exp " ^ string_of_exp e2 ^ " has type "
          ^ Type.to_string t
          ^ " but an expression of type Boolean was expected" )
    | t1, t2 ->
        failwith
          ( "R.type_check_prim: or exps " ^ string_of_exp e1 ^ " and "
          ^ string_of_exp e2 ^ " have types " ^ Type.to_string t1 ^ " and "
          ^ Type.to_string t2
          ^ " but expressions of type Boolean were expected" ) )

type answer = [`Int of int | `Bool of bool]

let rec interp ?(read = None) = function
  | Program (_, exp) ->
      type_check_exp empty_var_env exp |> ignore;
      interp_exp empty_var_env exp ~read

and interp_exp ?(read = None) env = function
  | Int i -> `Int i
  | Bool b -> `Bool b
  | Prim p -> interp_prim env p ~read
  | Var v -> (
    match Map.find env v with
    | None -> failwith ("R.interp_exp: var " ^ v ^ " is not bound")
    | Some e -> e )
  | Let (v, e1, e2) ->
      let e1 = interp_exp env e1 ~read in
      interp_exp (Map.set env v e1) e2 ~read
  | If (e1, e2, e3) -> (
    match interp_exp env e1 ~read with
    | `Bool true -> interp_exp env e2 ~read
    | `Bool false -> interp_exp env e3 ~read
    | _ -> assert false )

and interp_prim ?(read = None) env = function
  | Read -> (
    match read with
    | Some i -> `Int i
    | None -> `Int (read_int ()) )
  | Minus e -> (
    match interp_exp env e ~read with
    | `Int i -> `Int (-i)
    | _ -> assert false )
  | Plus (e1, e2) -> (
      let a1 = interp_exp env e1 ~read in
      let a2 = interp_exp env e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int (i1 + i2)
      | _ -> assert false )
  | Subtract (e1, e2) -> (
      let a1 = interp_exp env e1 ~read in
      let a2 = interp_exp env e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int (i1 - i2)
      | _ -> assert false )
  | Mult (e1, e2) -> (
      let a1 = interp_exp env e1 ~read in
      let a2 = interp_exp env e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int (i1 * i2)
      | _ -> assert false )
  | Div (e1, e2) -> (
      let a1 = interp_exp env e1 ~read in
      let a2 = interp_exp env e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int (i1 / i2)
      | _ -> assert false )
  | Rem (e1, e2) -> (
      let a1 = interp_exp env e1 ~read in
      let a2 = interp_exp env e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int (i1 mod i2)
      | _ -> assert false )
  | Land (e1, e2) -> (
      let a1 = interp_exp env e1 ~read in
      let a2 = interp_exp env e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int (i1 land i2)
      | _ -> assert false )
  | Lor (e1, e2) -> (
      let a1 = interp_exp env e1 ~read in
      let a2 = interp_exp env e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int (i1 lor i2)
      | _ -> assert false )
  | Lxor (e1, e2) -> (
      let a1 = interp_exp env e1 ~read in
      let a2 = interp_exp env e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Int (i1 lxor i2)
      | _ -> assert false )
  | Lnot e -> (
    match interp_exp env e ~read with
    | `Int i -> `Int (lnot i)
    | _ -> assert false )
  | Eq (e1, e2) -> (
      let a1 = interp_exp env e1 ~read in
      let a2 = interp_exp env e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Bool (i1 = i2)
      | `Bool b1, `Bool b2 -> `Bool (Bool.equal b1 b2)
      | _ -> assert false )
  | Lt (e1, e2) -> (
      let a1 = interp_exp env e1 ~read in
      let a2 = interp_exp env e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Bool (i1 < i2)
      | _ -> assert false )
  | Le (e1, e2) -> (
      let a1 = interp_exp env e1 ~read in
      let a2 = interp_exp env e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Bool (i1 <= i2)
      | _ -> assert false )
  | Gt (e1, e2) -> (
      let a1 = interp_exp env e1 ~read in
      let a2 = interp_exp env e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Bool (i1 > i2)
      | _ -> assert false )
  | Ge (e1, e2) -> (
      let a1 = interp_exp env e1 ~read in
      let a2 = interp_exp env e2 ~read in
      match (a1, a2) with
      | `Int i1, `Int i2 -> `Bool (i1 >= i2)
      | _ -> assert false )
  | Not e -> (
    match interp_exp env e ~read with
    | `Bool b -> `Bool (not b)
    | _ -> assert false )
  | And (e1, e2) -> (
      let a1 = interp_exp env e1 ~read in
      let a2 = interp_exp env e2 ~read in
      match (a1, a2) with
      | `Bool b1, `Bool b2 -> `Bool (b1 && b2)
      | _ -> assert false )
  | Or (e1, e2) -> (
      let a1 = interp_exp env e1 ~read in
      let a2 = interp_exp env e2 ~read in
      match (a1, a2) with
      | `Bool b1, `Bool b2 -> `Bool (b1 || b2)
      | _ -> assert false )

let rec uniquify = function
  | Program (info, e) ->
      let _, e = uniquify_exp empty_var_env e in
      Program (info, e)

and uniquify_exp m = function
  | Int _ as i -> (m, i)
  | Bool _ as b -> (m, b)
  | Prim p ->
      let _, p = uniquify_prim m p in
      (m, Prim p)
  | Var v -> (
    match Map.find m v with
    | None -> failwith ("R.uniquify_exp: var " ^ v ^ " is not bound")
    | Some n -> (m, Var (newvar v n)) )
  | Let (v, e1, e2) ->
      let n =
        match Map.find m v with
        | None -> 1
        | Some n -> n + 1
      in
      let m' = Map.set m v n in
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m' e2 in
      (m, Let (newvar v n, e1, e2))
  | If (e1, e2, e3) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      let _, e3 = uniquify_exp m e3 in
      (m, If (e1, e2, e3))

and uniquify_prim m = function
  | Read -> (m, Read)
  | Minus e ->
      let _, e = uniquify_exp m e in
      (m, Minus e)
  | Plus (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Plus (e1, e2))
  | Subtract (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Subtract (e1, e2))
  | Mult (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Mult (e1, e2))
  | Div (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Div (e1, e2))
  | Rem (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Rem (e1, e2))
  | Land (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Land (e1, e2))
  | Lor (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Lor (e1, e2))
  | Lxor (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Lxor (e1, e2))
  | Lnot e ->
      let _, e = uniquify_exp m e in
      (m, Lnot e)
  | Eq (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Eq (e1, e2))
  | Lt (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Lt (e1, e2))
  | Le (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Le (e1, e2))
  | Gt (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Gt (e1, e2))
  | Ge (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Ge (e1, e2))
  | Not e ->
      let _, e = uniquify_exp m e in
      (m, Not e)
  | And (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, And (e1, e2))
  | Or (e1, e2) ->
      let _, e1 = uniquify_exp m e1 in
      let _, e2 = uniquify_exp m e2 in
      (m, Or (e1, e2))

and newvar v n = Printf.sprintf "%s.%d" v n

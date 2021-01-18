open Core_kernel

type var = R.var

module Type = R.Type

let free_ptr = "_free_ptr"

let fromspace_end = "_fromspace_end"

type info = {typ: Type.t; nvars: int}

type t = Program of info * exp

and exp =
  | Int of int
  | Bool of bool
  | Void
  | Prim of prim
  | Var of var
  | Let of var * exp * exp
  | If of exp * exp * exp
  | Hastype of exp * Type.t
  | Collect of int
  | Allocate of int * Type.t
  | Globalvalue of string

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
  | Vectorlength of exp
  | Vectorref of exp * int
  | Vectorset of exp * int * exp

let rec to_string ?(has_type = false) = function
  | Program (_, exp) -> string_of_exp exp ~has_type

and string_of_exp ?(has_type = false) = function
  | Int i -> Int.to_string i
  | Bool b -> if b then "#t" else "#f"
  | Void -> "(void)"
  | Prim p -> string_of_prim p ~has_type
  | Var v -> v
  | Let (v, e1, e2) ->
      Printf.sprintf "(let ([%s %s]) %s)" v
        (string_of_exp e1 ~has_type)
        (string_of_exp e2 ~has_type)
  | If (e1, e2, e3) ->
      Printf.sprintf "(if %s %s %s)"
        (string_of_exp e1 ~has_type)
        (string_of_exp e2 ~has_type)
        (string_of_exp e3 ~has_type)
  | Hastype (e, t) ->
      if has_type then
        Printf.sprintf "(has-type %s %s)"
          (string_of_exp e ~has_type)
          (Type.to_string t)
      else string_of_exp e ~has_type
  | Collect n -> Printf.sprintf "(collect %d)" n
  | Allocate (n, t) -> Printf.sprintf "(allocate %d %s)" n (Type.to_string t)
  | Globalvalue v -> Printf.sprintf "(global-value '%s)" v

and string_of_prim ?(has_type = false) = function
  | Read -> "(read)"
  | Minus e -> Printf.sprintf "(- %s)" (string_of_exp e ~has_type)
  | Plus (e1, e2) ->
      Printf.sprintf "(+ %s %s)"
        (string_of_exp e1 ~has_type)
        (string_of_exp e2 ~has_type)
  | Subtract (e1, e2) ->
      Printf.sprintf "(- %s %s)"
        (string_of_exp e1 ~has_type)
        (string_of_exp e2 ~has_type)
  | Mult (e1, e2) ->
      Printf.sprintf "(* %s %s)"
        (string_of_exp e1 ~has_type)
        (string_of_exp e2 ~has_type)
  | Div (e1, e2) ->
      Printf.sprintf "(/ %s %s)"
        (string_of_exp e1 ~has_type)
        (string_of_exp e2 ~has_type)
  | Rem (e1, e2) ->
      Printf.sprintf "(rem %s %s)"
        (string_of_exp e1 ~has_type)
        (string_of_exp e2 ~has_type)
  | Land (e1, e2) ->
      Printf.sprintf "(land %s %s)"
        (string_of_exp e1 ~has_type)
        (string_of_exp e2 ~has_type)
  | Lor (e1, e2) ->
      Printf.sprintf "(lor %s %s)"
        (string_of_exp e1 ~has_type)
        (string_of_exp e2 ~has_type)
  | Lxor (e1, e2) ->
      Printf.sprintf "(lxor %s %s)"
        (string_of_exp e1 ~has_type)
        (string_of_exp e2 ~has_type)
  | Lnot e -> Printf.sprintf "(lnot %s)" (string_of_exp e ~has_type)
  | Eq (e1, e2) ->
      Printf.sprintf "(eq? %s %s)"
        (string_of_exp e1 ~has_type)
        (string_of_exp e2 ~has_type)
  | Lt (e1, e2) ->
      Printf.sprintf "(< %s %s)"
        (string_of_exp e1 ~has_type)
        (string_of_exp e2 ~has_type)
  | Le (e1, e2) ->
      Printf.sprintf "(<= %s %s)"
        (string_of_exp e1 ~has_type)
        (string_of_exp e2 ~has_type)
  | Gt (e1, e2) ->
      Printf.sprintf "(> %s %s)"
        (string_of_exp e1 ~has_type)
        (string_of_exp e2 ~has_type)
  | Ge (e1, e2) ->
      Printf.sprintf "(>= %s %s)"
        (string_of_exp e1 ~has_type)
        (string_of_exp e2 ~has_type)
  | Not e -> Printf.sprintf "(not %s)" (string_of_exp e ~has_type)
  | And (e1, e2) ->
      Printf.sprintf "(and %s %s)"
        (string_of_exp e1 ~has_type)
        (string_of_exp e2 ~has_type)
  | Or (e1, e2) ->
      Printf.sprintf "(or %s %s)"
        (string_of_exp e1 ~has_type)
        (string_of_exp e2 ~has_type)
  | Vectorlength e ->
      Printf.sprintf "(vector-length %s)" (string_of_exp e ~has_type)
  | Vectorref (e, i) ->
      Printf.sprintf "(vector-ref %s %d)" (string_of_exp e ~has_type) i
  | Vectorset (e1, i, e2) ->
      Printf.sprintf "(vector-set! %s %d %s)"
        (string_of_exp e1 ~has_type)
        i
        (string_of_exp e2 ~has_type)

let newvar n =
  let v = Printf.sprintf "%%%d" !n in
  incr n; v

let rec expose_allocation = function
  | R.Program (info, exp) ->
      let n = ref 0 in
      let exp = expose_allocation_exp n exp in
      Program ({typ= info.typ; nvars= !n}, exp)

and expose_allocation_exp n = function
  | R.Int i -> Int i
  | R.Bool b -> Bool b
  | R.Void -> Void
  | R.Prim p -> Prim (expose_allocation_prim n p)
  | R.Var v -> Var v
  | R.Let (v, e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Let (v, e1, e2)
  | R.If (e1, e2, e3) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      let e3 = expose_allocation_exp n e3 in
      If (e1, e2, e3)
  | R.(Hastype (Prim (Vector es), (Type.Vector ts as t))) ->
      expand_alloc n t ts es
  | R.(Hastype (Prim (Vector _), _)) -> assert false
  | R.Hastype (e, t) -> Hastype (expose_allocation_exp n e, t)

and expand_alloc n t ts es =
  let v = newvar n in
  let len = List.length es in
  let vs = nvars n len in
  let es = List.map es ~f:(expose_allocation_exp n) in
  let expand base t =
    List.fold_right ~init:base ~f:(fun (v, e) acc ->
        Hastype (Let (v, e, acc), t))
  in
  let alloc =
    (* generate the sequence that initializes the contents of the vector *)
    let base =
      let vec = Hastype (Var v, t) in
      List.zip_exn vs ts
      |> List.foldi ~init:[] ~f:(fun i acc (v, t) ->
             Prim (Vectorset (vec, i, Hastype (Var v, t))) :: acc)
      |> List.rev
      |> List.zip_exn (nvars n len)
      |> expand vec t
    in
    (* hardcode the idiom that allocates the vector triggering
     * the GC beforehand if there is not enough space *)
    Let
      ( newvar n
      , If
          ( Prim
              (Lt
                 ( Prim
                     (Plus
                        ( Hastype (Globalvalue free_ptr, Type.Integer)
                        , Int (len lsl 3) ))
                 , Hastype (Globalvalue fromspace_end, Type.Integer) ))
          , Void
          , Collect (len lsl 3) )
      , Let (v, Allocate (len, t), base) )
  in
  (* evaluate the arguments before allocating *)
  List.zip_exn vs es |> expand alloc t

and expose_allocation_prim n = function
  | R.Read -> Read
  | R.Minus e -> Minus (expose_allocation_exp n e)
  | R.Plus (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Plus (e1, e2)
  | R.Subtract (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Subtract (e1, e2)
  | R.Mult (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Mult (e1, e2)
  | R.Div (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Div (e1, e2)
  | R.Rem (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Rem (e1, e2)
  | R.Land (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Land (e1, e2)
  | R.Lor (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Lor (e1, e2)
  | R.Lxor (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Lxor (e1, e2)
  | R.Lnot e -> Lnot (expose_allocation_exp n e)
  | R.Eq (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Eq (e1, e2)
  | R.Lt (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Lt (e1, e2)
  | R.Le (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Le (e1, e2)
  | R.Gt (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Gt (e1, e2)
  | R.Ge (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Ge (e1, e2)
  | R.Not e -> Not (expose_allocation_exp n e)
  | R.And (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      And (e1, e2)
  | R.Or (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Or (e1, e2)
  | R.Vector es -> assert false
  | R.Vectorlength e -> Vectorlength (expose_allocation_exp n e)
  | R.Vectorref (e, i) -> Vectorref (expose_allocation_exp n e, i)
  | R.Vectorset (e1, i, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Vectorset (e1, i, e2)

and nvars n = function
  | 0 -> []
  | i -> newvar n :: nvars n (pred i)

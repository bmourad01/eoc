open Core_kernel

type var = R_typed.var

module Type = R_typed.Type
module Type_map = R_typed.Type_map

type type_env = R_typed.type_env

let free_ptr = "_free_ptr"

let fromspace_end = "_fromspace_end"

let word_size = 8

let total_tag_offset = 1

type info = {nvars: int Label.Map.t}

type t = Program of info * def list

and def = Def of var * (var * Type.t) list * Type.t * exp

and exp =
  | Int of Int64.t
  | Bool of bool
  | Void
  | Prim of prim * Type.t
  | Var of var * Type.t
  | Let of var * exp * exp * Type.t
  | If of exp * exp * exp * Type.t
  | Apply of exp * exp list * Type.t
  | Funref of var * Type.t
  | Setbang of var * exp
  | Begin of exp list * exp * Type.t
  | While of exp * exp
  | Collect of int
  | Allocate of int * Type.t
  | Globalvalue of string * Type.t

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

let rec to_string = function
  | Program (_, defs) ->
      List.map defs ~f:string_of_def |> String.concat ~sep:"\n\n"

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
  | Prim (p, _) -> string_of_prim p
  | Var (v, _) -> v
  | Let (v, e1, e2, _) ->
      Printf.sprintf "(let ([%s %s]) %s)" v (string_of_exp e1)
        (string_of_exp e2)
  | If (e1, e2, e3, _) ->
      Printf.sprintf "(if %s %s %s)" (string_of_exp e1) (string_of_exp e2)
        (string_of_exp e3)
  | Apply (e, [], _) -> Printf.sprintf "(%s)" (string_of_exp e)
  | Apply (e, es, _) ->
      Printf.sprintf "(%s %s)" (string_of_exp e)
        (List.map es ~f:string_of_exp |> String.concat ~sep:" ")
  | Funref (v, _) -> Printf.sprintf "(fun-ref %s)" v
  | Setbang (v, e) -> Printf.sprintf "(set! %s %s)" v (string_of_exp e)
  | Begin ([], e, _) -> Printf.sprintf "(begin %s)" (string_of_exp e)
  | Begin (es, e, _) ->
      Printf.sprintf "(begin %s %s)"
        (List.map es ~f:string_of_exp |> String.concat ~sep:" ")
        (string_of_exp e)
  | While (e1, e2) ->
      Printf.sprintf "(while %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Collect n -> Printf.sprintf "(collect %d)" n
  | Allocate (n, t) -> Printf.sprintf "(allocate %d %s)" n (Type.to_string t)
  | Globalvalue (v, _) -> Printf.sprintf "(global-value '%s)" v

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
  | Vectorlength e -> Printf.sprintf "(vector-length %s)" (string_of_exp e)
  | Vectorref (e, i) ->
      Printf.sprintf "(vector-ref %s %d)" (string_of_exp e) i
  | Vectorset (e1, i, e2) ->
      Printf.sprintf "(vector-set! %s %d %s)" (string_of_exp e1) i
        (string_of_exp e2)

let newvar n =
  let v = Printf.sprintf "%%%d" !n in
  incr n; v

let rec expose_allocation = function
  | R_typed.Program (info, defs) ->
      let defs, ns = List.map defs ~f:expose_allocation_def |> List.unzip in
      Program ({nvars= Label.Map.of_alist_exn ns}, defs)

and expose_allocation_def = function
  | R_typed.Def (v, args, t, e) ->
      let n = ref 0 in
      let e = expose_allocation_exp n e in
      (Def (v, args, t, e), (v, !n))

and expose_allocation_exp n = function
  | R_typed.Int i -> Int i
  | R_typed.Bool b -> Bool b
  | R_typed.Void -> Void
  | R_typed.Prim (Vector es, (Type.Vector ts as t)) -> expand_alloc n t ts es
  | R_typed.Prim (p, t) -> Prim (expose_allocation_prim n p, t)
  | R_typed.Var (v, t) -> Var (v, t)
  | R_typed.Let (v, e1, e2, t) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Let (v, e1, e2, t)
  | R_typed.If (e1, e2, e3, t) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      let e3 = expose_allocation_exp n e3 in
      If (e1, e2, e3, t)
  | R_typed.Apply (e, es, t) ->
      let e = expose_allocation_exp n e in
      let es = List.map es ~f:(expose_allocation_exp n) in
      Apply (e, es, t)
  | R_typed.Funref (v, t) -> Funref (v, t)
  | R_typed.Lambda _ -> assert false
  | R_typed.Setbang (v, e) -> Setbang (v, expose_allocation_exp n e)
  | R_typed.Begin (es, e, t) ->
      Begin
        ( List.map es ~f:(expose_allocation_exp n)
        , expose_allocation_exp n e
        , t )
  | R_typed.While (e1, e2) ->
      While (expose_allocation_exp n e1, expose_allocation_exp n e2)

and expand_alloc n t ts es =
  let v = newvar n in
  let len = List.length es in
  let vs = nvars n len in
  let es = List.map es ~f:(expose_allocation_exp n) in
  let expand base t =
    List.fold_right ~init:base ~f:(fun (v, e) acc -> Let (v, e, acc, t))
  in
  let alloc =
    (* generate the sequence that initializes the contents of the vector *)
    let base =
      let vec = Var (v, t) in
      let sets =
        List.zip_exn vs ts
        |> List.foldi ~init:[] ~f:(fun i acc (v, t) ->
               Prim (Vectorset (vec, i, Var (v, t)), Type.Void) :: acc)
        |> List.rev
      in
      Begin (sets, vec, t)
    in
    (* hardcode the idiom that allocates the vector triggering
     * the GC beforehand if there is not enough space
     *
     * XXX: can we do this with `begin`? something goes wrong
     * with explicate-control when we try it
     * (perhaps not well-formed?) *)
    let bytes = (len + total_tag_offset) * word_size in
    Let
      ( newvar n
      , If
          ( Prim
              ( Lt
                  ( Prim
                      ( Plus
                          ( Globalvalue (free_ptr, Type.Integer)
                          , Int (Int64.of_int bytes) )
                      , Type.Integer )
                  , Globalvalue (fromspace_end, Type.Integer) )
              , Type.Boolean )
          , Void
          , Collect bytes
          , Type.Void )
      , Let (v, Allocate (len, t), base, t)
      , t )
  in
  (* evaluate the arguments before allocating *)
  List.zip_exn vs es |> expand alloc t

and expose_allocation_prim n = function
  | R_typed.Read -> Read
  | R_typed.Minus e -> Minus (expose_allocation_exp n e)
  | R_typed.Plus (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Plus (e1, e2)
  | R_typed.Subtract (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Subtract (e1, e2)
  | R_typed.Mult (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Mult (e1, e2)
  | R_typed.Div (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Div (e1, e2)
  | R_typed.Rem (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Rem (e1, e2)
  | R_typed.Land (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Land (e1, e2)
  | R_typed.Lor (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Lor (e1, e2)
  | R_typed.Lxor (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Lxor (e1, e2)
  | R_typed.Lnot e -> Lnot (expose_allocation_exp n e)
  | R_typed.Eq (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Eq (e1, e2)
  | R_typed.Lt (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Lt (e1, e2)
  | R_typed.Le (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Le (e1, e2)
  | R_typed.Gt (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Gt (e1, e2)
  | R_typed.Ge (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Ge (e1, e2)
  | R_typed.Not e -> Not (expose_allocation_exp n e)
  | R_typed.And (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      And (e1, e2)
  | R_typed.Or (e1, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Or (e1, e2)
  | R_typed.Vector es -> assert false
  | R_typed.Vectorlength e -> Vectorlength (expose_allocation_exp n e)
  | R_typed.Vectorref (e, i) -> Vectorref (expose_allocation_exp n e, i)
  | R_typed.Vectorset (e1, i, e2) ->
      let e1 = expose_allocation_exp n e1 in
      let e2 = expose_allocation_exp n e2 in
      Vectorset (e1, i, e2)

and nvars n = function
  | 0 -> []
  | i -> newvar n :: nvars n (pred i)

open Core_kernel
module Type = R_anf.Type
module Type_map = R_anf.Type_map

type type_env = R_anf.type_env

type var = R.var

type info = {main: Label.t}

module Cmp = struct
  type t = Eq | Lt | Le | Gt | Ge

  let to_string = function
    | Eq -> "eq?"
    | Lt -> "<"
    | Le -> "<="
    | Gt -> ">"
    | Ge -> ">="
end

module Cfg = Cfg.Make (Label)

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

and exp =
  | Atom of atom
  | Prim of prim * Type.t
  | Funref of Label.t * Type.t
  | Call of atom * atom list * Type.t
  | Allocate of int * Type.t
  | Globalvalue of string * Type.t
  | Select of cmp * atom * atom * Type.t

and atom = Int of Int64.t | Bool of bool | Var of var * Type.t | Void

and prim =
  | Read
  | Minus of atom
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
  | Lt of atom * atom
  | Le of atom * atom
  | Gt of atom * atom
  | Ge of atom * atom
  | Not of atom
  | Vectorlength of atom
  | Vectorref of atom * int

and cmp = Cmp.t * atom * atom

let typeof_exp = function
  | Atom (Int _) -> Type.Integer
  | Atom (Bool _) -> Type.Boolean
  | Atom (Var (_, t)) -> t
  | Atom Void -> Type.Void
  | Prim (_, t) -> t
  | Funref (_, t) -> t
  | Call (_, _, t) -> t
  | Allocate (_, t) -> t
  | Globalvalue (_, t) -> t
  | Select (_, _, _, t) -> t

let rec to_string = function
  | Program (_, defs) ->
      List.map defs ~f:string_of_def |> String.concat ~sep:"\n\n"

and string_of_def = function
  | Def (v, args, t, info, tails) ->
      let s =
        List.map args ~f:(fun (a, t) ->
            Printf.sprintf "[%s : %s]" a (Type.to_string t))
        |> String.concat ~sep:" "
      in
      let tls =
        List.map tails ~f:(fun (l, t) ->
            Printf.sprintf "(%s:\n  %s)" l (string_of_tail t))
        |> String.concat ~sep:"\n "
      in
      if String.is_empty s then
        Printf.sprintf "(define (%s) : %s\n %s)" v (Type.to_string t) tls
      else
        Printf.sprintf "(define (%s %s) : %s\n %s)" v s (Type.to_string t)
          tls

and string_of_tail = function
  | Return e -> Printf.sprintf "(return %s)" (string_of_exp e)
  | Seq (s, t) ->
      Printf.sprintf "%s\n  %s" (string_of_stmt s) (string_of_tail t)
  | Goto l -> Printf.sprintf "(goto %s)" l
  | If (cmp, lt, lf) ->
      Printf.sprintf "(if %s (goto %s) (goto %s))" (string_of_cmp cmp) lt lf
  | Tailcall (a, [], _) -> Printf.sprintf "(tail-call %s)" (string_of_atom a)
  | Tailcall (a, as', _) ->
      Printf.sprintf "(tail-call %s %s)" (string_of_atom a)
        (List.map as' ~f:string_of_atom |> String.concat ~sep:" ")

and string_of_stmt = function
  | Assign (v, e) -> Printf.sprintf "(set! %s %s)" v (string_of_exp e)
  | Collect n -> Printf.sprintf "(collect %d)" n
  | Callstmt (a, []) -> Printf.sprintf "(call %s)" (string_of_atom a)
  | Callstmt (a, as') ->
      Printf.sprintf "(call %s %s)" (string_of_atom a)
        (List.map as' ~f:string_of_atom |> String.concat ~sep:" ")
  | Vectorsetstmt (a1, i, a2) ->
      Printf.sprintf "(vector-set! %s %d %s)" (string_of_atom a1) i
        (string_of_atom a2)
  | Readstmt -> "(read)"
  | Printstmt a -> Printf.sprintf "(print %s)" (string_of_atom a)

and string_of_exp = function
  | Atom a -> string_of_atom a
  | Prim (p, _) -> string_of_prim p
  | Funref (l, _) -> Printf.sprintf "(fun-ref %s)" l
  | Call (a, [], _) -> Printf.sprintf "(call %s)" (string_of_atom a)
  | Call (a, as', _) ->
      Printf.sprintf "(call %s %s)" (string_of_atom a)
        (List.map as' ~f:string_of_atom |> String.concat ~sep:" ")
  | Allocate (n, t) -> Printf.sprintf "(allocate %d %s)" n (Type.to_string t)
  | Globalvalue (v, _) -> Printf.sprintf "(global-value '%s)" v
  | Select (cmp, a1, a2, _) ->
      Printf.sprintf "(select %s %s %s)" (string_of_cmp cmp)
        (string_of_atom a1) (string_of_atom a2)

and string_of_atom = function
  | Int i -> Int64.to_string i
  | Bool b -> if b then "#t" else "#f"
  | Var (v, _) -> v
  | Void -> "(void)"

and string_of_prim = function
  | Read -> "(read)"
  | Minus a -> Printf.sprintf "(- %s)" (string_of_atom a)
  | Plus (a1, a2) ->
      Printf.sprintf "(+ %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Subtract (a1, a2) ->
      Printf.sprintf "(- %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Mult (a1, a2) ->
      Printf.sprintf "(* %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Div (a1, a2) ->
      Printf.sprintf "(/ %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Rem (a1, a2) ->
      Printf.sprintf "(rem %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Land (a1, a2) ->
      Printf.sprintf "(land %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Lor (a1, a2) ->
      Printf.sprintf "(lor %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Lxor (a1, a2) ->
      Printf.sprintf "(lxor %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Lnot a -> Printf.sprintf "(lnot %s)" (string_of_atom a)
  | Eq (a1, a2) ->
      Printf.sprintf "(eq? %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Lt (a1, a2) ->
      Printf.sprintf "(< %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Le (a1, a2) ->
      Printf.sprintf "(<= %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Gt (a1, a2) ->
      Printf.sprintf "(> %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Ge (a1, a2) ->
      Printf.sprintf "(>= %s %s)" (string_of_atom a1) (string_of_atom a2)
  | Not a -> Printf.sprintf "(not %s)" (string_of_atom a)
  | Vectorlength a -> Printf.sprintf "(vector-length %s)" (string_of_atom a)
  | Vectorref (a, i) ->
      Printf.sprintf "(vector-ref %s %d)" (string_of_atom a) i

and string_of_cmp (cmp, a1, a2) =
  Printf.sprintf "(%s %s %s)" (Cmp.to_string cmp) (string_of_atom a1)
    (string_of_atom a2)

let read_int () =
  Out_channel.(flush stdout);
  Int64.of_string In_channel.(input_line_exn stdin)

type answer = R_typed.answer

let string_of_answer = R_typed.string_of_answer

let rec interp ?(read = None) = function
  | Program (info, defs) -> (
      let (Def (_, _, _, info', tails)) =
        List.find_exn defs ~f:(function Def (v, _, _, _, _) ->
            Label.equal v info.main)
      in
      let tails = Label.Map.of_alist_exn tails in
      match Map.find tails info'.main with
      | None -> failwith "C.interp: no main label defined"
      | Some t -> interp_tail (ref R_typed.empty_var_env) defs tails t ~read
      )

and interp_tail ?(read = None) env defs tails = function
  | Return e -> interp_exp env defs e ~read
  | Seq (s, t) ->
      interp_stmt env defs s ~read;
      interp_tail env defs tails t ~read
  | Goto l -> (
    match Map.find tails l with
    | None -> failwith ("C.interp: goto label " ^ l ^ " does not exist")
    | Some t -> interp_tail env defs tails t ~read )
  | If (cmp, lt, lf) -> (
    match interp_cmp env defs cmp with
    | `Bool b ->
        let t = if b then Goto lt else Goto lf in
        interp_tail env defs tails t ~read
    | _ -> assert false )
  | Tailcall (a, as', _) -> (
    match interp_atom env defs a ~read with
    | `Def v -> (
        let (Def (_, args, _, info', tails)) =
          List.find_exn defs ~f:(function Def (v', _, _, _, _) ->
              Label.equal v v')
        in
        let as' = List.map as' ~f:(interp_atom env defs ~read) in
        let env =
          List.zip_exn (List.map args ~f:fst) as'
          |> List.fold ~init:!env ~f:(fun env (x, a) -> Map.set env x a)
          |> ref
        in
        let tails = Label.Map.of_alist_exn tails in
        match Map.find tails info'.main with
        | None ->
            failwith ("C.interp: no entry label defined for function " ^ v)
        | Some t -> interp_tail env defs tails t ~read )
    | _ -> assert false )

and interp_stmt ?(read = None) env defs = function
  | Assign (v, e) ->
      let e = interp_exp env defs e ~read in
      env := Map.set !env v e
  | Collect _ -> ()
  | Callstmt (a, as') -> (
    match interp_atom env defs a ~read with
    | `Def v ->
        let (Def (_, args, _, info', tails)) =
          List.find_exn defs ~f:(function Def (v', _, _, _, _) ->
              Label.equal v v')
        in
        let as' = List.map as' ~f:(interp_atom env defs ~read) in
        let env =
          List.zip_exn (List.map args ~f:fst) as'
          |> String.Map.of_alist_exn |> ref
        in
        let tails = Label.Map.of_alist_exn tails in
        ( match Map.find tails info'.main with
        | None ->
            failwith ("C.interp: no entry label defined for function " ^ v)
        | Some t -> interp_tail env defs tails t ~read )
        |> ignore
    | _ -> assert false )
  | Vectorsetstmt (a1, i, a2) -> (
    match (interp_atom env defs a1, interp_atom env defs a2) with
    | `Vector v, v' -> v.(i) <- v'
    | _ -> assert false )
  | Readstmt -> (
    match read with
    | None -> read_int () |> ignore
    | Some _ -> () )
  | Printstmt a ->
      interp_atom env defs a ~read |> string_of_answer |> print_endline

and interp_exp ?(read = None) env defs = function
  | Atom a -> interp_atom env defs a
  | Prim (p, _) -> interp_prim env defs p ~read
  | Funref (v, _) -> `Def v
  | Call (a, as', _) -> (
    match interp_atom env defs a ~read with
    | `Def v -> (
        let (Def (_, args, _, info', tails)) =
          List.find_exn defs ~f:(function Def (v', _, _, _, _) ->
              Label.equal v v')
        in
        let as' = List.map as' ~f:(interp_atom env defs ~read) in
        let env =
          List.zip_exn (List.map args ~f:fst) as'
          |> String.Map.of_alist_exn |> ref
        in
        let tails = Label.Map.of_alist_exn tails in
        match Map.find tails info'.main with
        | None ->
            failwith ("C.interp: no entry label defined for function " ^ v)
        | Some t -> interp_tail env defs tails t ~read )
    | _ -> assert false )
  | Allocate (n, _) -> `Vector (Array.init n ~f:(fun _ -> `Void))
  | Globalvalue _ -> `Int 0L
  | Select (cmp, a1, a2, _) -> (
    match interp_cmp env defs cmp ~read with
    | `Bool true -> interp_atom env defs a1 ~read
    | `Bool false -> interp_atom env defs a2 ~read
    | _ -> assert false )

and interp_atom ?(read = None) env defs = function
  | Int i -> `Int i
  | Bool b -> `Bool b
  | Var (v, _) -> (
    match Map.find !env v with
    | None -> failwith ("C.interp_atom: var " ^ v ^ " is unbound")
    | Some i -> i )
  | Void -> `Void

and interp_prim ?(read = None) env defs = function
  | Read -> (
    match read with
    | Some i -> `Int i
    | None -> `Int (read_int ()) )
  | Minus a -> (
    match interp_atom env defs a with
    | `Int i -> `Int Int64.(-i)
    | _ -> assert false )
  | Plus (a1, a2) -> (
    match (interp_atom env defs a1, interp_atom env defs a2) with
    | `Int i1, `Int i2 -> `Int Int64.(i1 + i2)
    | a1, a2 -> assert false )
  | Subtract (a1, a2) -> (
    match (interp_atom env defs a1, interp_atom env defs a2) with
    | `Int i1, `Int i2 -> `Int Int64.(i1 - i2)
    | _ -> assert false )
  | Mult (a1, a2) -> (
    match (interp_atom env defs a1, interp_atom env defs a2) with
    | `Int i1, `Int i2 -> `Int Int64.(i1 * i2)
    | _ -> assert false )
  | Div (a1, a2) -> (
    match (interp_atom env defs a1, interp_atom env defs a2) with
    | `Int i1, `Int i2 -> `Int Int64.(i1 / i2)
    | _ -> assert false )
  | Rem (a1, a2) -> (
    match (interp_atom env defs a1, interp_atom env defs a2) with
    | `Int i1, `Int i2 -> `Int Int64.(rem i1 i2)
    | _ -> assert false )
  | Land (a1, a2) -> (
    match (interp_atom env defs a1, interp_atom env defs a2) with
    | `Int i1, `Int i2 -> `Int Int64.(i1 land i2)
    | _ -> assert false )
  | Lor (a1, a2) -> (
    match (interp_atom env defs a1, interp_atom env defs a2) with
    | `Int i1, `Int i2 -> `Int Int64.(i1 lor i2)
    | _ -> assert false )
  | Lxor (a1, a2) -> (
    match (interp_atom env defs a1, interp_atom env defs a2) with
    | `Int i1, `Int i2 -> `Int Int64.(i1 lxor i2)
    | _ -> assert false )
  | Lnot a -> (
    match interp_atom env defs a with
    | `Int i -> `Int Int64.(lnot i)
    | _ -> assert false )
  | Eq (a1, a2) -> (
    match (interp_atom env defs a1, interp_atom env defs a2) with
    | `Int i1, `Int i2 -> `Bool Int64.(i1 = i2)
    | `Bool b1, `Bool b2 -> `Bool (Bool.equal b1 b2)
    | _ -> assert false )
  | Lt (a1, a2) -> (
    match (interp_atom env defs a1, interp_atom env defs a2) with
    | `Int i1, `Int i2 -> `Bool Int64.(i1 < i2)
    | _ -> assert false )
  | Le (a1, a2) -> (
    match (interp_atom env defs a1, interp_atom env defs a2) with
    | `Int i1, `Int i2 -> `Bool Int64.(i1 <= i2)
    | _ -> assert false )
  | Gt (a1, a2) -> (
    match (interp_atom env defs a1, interp_atom env defs a2) with
    | `Int i1, `Int i2 -> `Bool Int64.(i1 > i2)
    | _ -> assert false )
  | Ge (a1, a2) -> (
    match (interp_atom env defs a1, interp_atom env defs a2) with
    | `Int i1, `Int i2 -> `Bool Int64.(i1 >= i2)
    | _ -> assert false )
  | Not a -> (
    match interp_atom env defs a with
    | `Bool b -> `Bool (not b)
    | _ -> assert false )
  | Vectorlength a -> (
    match interp_atom env defs a with
    | `Vector v -> `Int Int64.(Array.length v |> of_int)
    | _ -> assert false )
  | Vectorref (a, i) -> (
    match interp_atom env defs a with
    | `Vector v -> v.(i)
    | _ -> assert false )

and interp_cmp ?(read = None) env defs (cmp, a1, a2) =
  let e =
    match cmp with
    | Cmp.Eq -> Prim (Eq (a1, a2), Type.Boolean)
    | Cmp.Lt -> Prim (Lt (a1, a2), Type.Boolean)
    | Cmp.Le -> Prim (Le (a1, a2), Type.Boolean)
    | Cmp.Gt -> Prim (Gt (a1, a2), Type.Boolean)
    | Cmp.Ge -> Prim (Ge (a1, a2), Type.Boolean)
  in
  interp_exp env defs e ~read

let rec explicate_control = function
  | R_anf.Program (info, defs) ->
      Program
        ( {main= R_typed.main}
        , List.map defs ~f:(explicate_control_def info.nvars) )

and explicate_control_def nvars = function
  | R_anf.Def (v, args, t, e) ->
      (* compile to a C program by flattening control flow *)
      let tails = Hashtbl.create (module Label) in
      let nv = ref (Map.find_exn nvars v) in
      Hashtbl.set tails v (explicate_tail v tails nv (ref 0) e);
      (* construct the CFG *)
      let cfg =
        Hashtbl.fold tails
          ~init:Cfg.(add_vertex empty v)
          ~f:(fun ~key:label ~data:tail cfg ->
            let rec aux = function
              | Return _ -> cfg
              | Seq (_, t) -> aux t
              | Goto l -> Cfg.add_edge cfg label l
              | If (_, lt, lf) ->
                  Cfg.(add_edge (add_edge cfg label lt) label lf)
              | Tailcall _ -> cfg
            in
            aux tail)
      in
      (* reorder the tails according to a reverse post-order
       * DFS traversal, starting from the entry node. this will
       * give a structure to the program that is amenable to
       * optimizing jumps later when we compile to X. *)
      let rec traverse_dfs_post v s res =
        let s, res =
          Cfg.fold_succ
            (fun v (s, res) ->
              if Set.mem s v then (s, res)
              else traverse_dfs_post v Set.(add s v) res)
            cfg v (s, res)
        in
        (s, v :: res)
      in
      let visited, labels = traverse_dfs_post v Label.Set.(singleton v) [] in
      let tails =
        List.fold_right labels ~init:[] ~f:(fun l acc ->
            (l, Hashtbl.find_exn tails l) :: acc)
      in
      (* purge unreachable nodes from the CFG *)
      let cfg =
        Cfg.fold_vertex
          (fun v acc ->
            if Set.mem visited v then acc else Cfg.remove_vertex acc v)
          cfg cfg
      in
      (* map local variables to their types *)
      let locals_types =
        List.fold args ~init:String.Map.empty ~f:(fun locals_types (x, t) ->
            Map.set locals_types x t)
      in
      let locals_types =
        List.fold tails ~init:locals_types ~f:(fun locals_types (_, tail) ->
            let rec aux_tail env = function
              | Return e -> env
              | Seq (s, t) -> aux_tail (aux_stmt env s) t
              | Goto _ -> env
              | If _ -> env
              | Tailcall _ -> env
            and aux_stmt env = function
              | Assign (v, e) -> Map.set env v (typeof_exp e)
              | Collect _ -> env
              | Callstmt _ -> env
              | Vectorsetstmt _ -> env
              | Readstmt -> env
              | Printstmt _ -> env
            in
            aux_tail locals_types tail)
      in
      Def
        ( v
        , args
        , t
        , {main= List.hd_exn tails |> fst; cfg; locals_types}
        , tails )

and explicate_tail fn tails nv n = function
  | R_anf.(Atom a) -> Return (Atom (translate_atom a))
  | R_anf.(Prim (Print a, _)) ->
      Seq (Printstmt (translate_atom a), Return (Atom Void))
  | R_anf.(Prim (Vectorset (a1, i, a2), _)) ->
      Seq
        ( Vectorsetstmt (translate_atom a1, i, translate_atom a2)
        , Return (Atom Void) )
  | R_anf.(Prim (p, t)) -> Return (Prim (translate_prim p, t))
  (* match the pattern for a conditional move *)
  | R_anf.(Let (v, If (econd, (Atom _ as e1), (Atom _ as e2), t), ebody, _))
    ->
      let cont = explicate_tail fn tails nv n ebody in
      let t = do_select fn tails nv n econd e1 e2 t in
      do_assign fn tails nv n t v cont
  | R_anf.(Let (v, e1, e2, _)) ->
      let cont = explicate_tail fn tails nv n e2 in
      explicate_assign fn tails nv n e1 v cont
  (* match the pattern for a conditional move *)
  | R_anf.(If (econd, (Atom _ as e1), (Atom _ as e2), t)) ->
      do_select fn tails nv n econd e1 e2 t
  | R_anf.(If (e1, e2, e3, _)) ->
      let tt = explicate_tail fn tails nv n e2 in
      let tf = explicate_tail fn tails nv n e3 in
      explicate_pred fn tails nv n e1 tt tf
  | R_anf.(Apply (a, as', t)) ->
      if Label.equal fn R_typed.main then
        (* the main function (our top-level program) should
         * never make tail calls. all of them are expected
         * to return. *)
        Return (Call (translate_atom a, List.map as' ~f:translate_atom, t))
      else Tailcall (translate_atom a, List.map as' ~f:translate_atom, t)
  | R_anf.(Funref (v, t)) -> Return (Funref (v, t))
  | R_anf.Setbang (v, e) ->
      explicate_assign fn tails nv n e v (Return (Atom Void))
  | R_anf.Begin (es, e, _) ->
      List.fold_right es
        ~init:(explicate_tail fn tails nv n e)
        ~f:(explicate_effect fn tails nv n)
  | R_anf.While (e1, e2) ->
      let loop = fresh_label fn n in
      let tt = explicate_effect fn tails nv n e2 (Goto loop) in
      let top = explicate_pred fn tails nv n e1 tt (Return (Atom Void)) in
      add_tail tails loop top; Goto loop
  | R_anf.Collect n -> Seq (Collect n, Return (Atom Void))
  | R_anf.Allocate (n, t) -> Return (Allocate (n, t))
  | R_anf.Globalvalue (v, t) -> Return (Globalvalue (v, t))

and explicate_select fn tails nv n cmp e1 e2 t =
  let e1_var, x1 =
    match e1 with
    | R_anf.(Atom (Var (x, _))) -> (true, x)
    | _ -> (false, fresh_var nv)
  in
  let e2_var, x2 =
    match e2 with
    | R_anf.(Atom (Var (x, _))) -> (true, x)
    | _ -> (false, fresh_var nv)
  in
  let cont = Return (Select (cmp, Var (x1, t), Var (x2, t), t)) in
  let cont =
    if e1_var then cont else explicate_assign fn tails nv n e2 x2 cont
  in
  if e2_var then cont else explicate_assign fn tails nv n e1 x1 cont

(* actually generate the select *)
and do_select fn tails nv n econd e1 e2 t =
  let sel cmp = explicate_select fn tails nv n cmp e1 e2 t in
  match econd with
  | R_anf.(Atom (Bool true)) -> (
    match e1 with
    | R_anf.Atom a -> Return (Atom (translate_atom a))
    | _ -> assert false )
  | R_anf.(Atom (Bool false)) -> (
    match e2 with
    | R_anf.Atom a -> Return (Atom (translate_atom a))
    | _ -> assert false )
  | R_anf.(Atom (Var (x, t'))) -> sel (Cmp.Eq, Var (x, t'), Bool true)
  | R_anf.(Prim (Not a, _)) -> sel (Cmp.Eq, translate_atom a, Bool false)
  | R_anf.(Prim (Eq (a1, a2), _)) ->
      sel (Cmp.Eq, translate_atom a1, translate_atom a2)
  | R_anf.(Prim (Lt (a1, a2), _)) ->
      sel (Cmp.Lt, translate_atom a1, translate_atom a2)
  | R_anf.(Prim (Le (a1, a2), _)) ->
      sel (Cmp.Le, translate_atom a1, translate_atom a2)
  | R_anf.(Prim (Gt (a1, a2), _)) ->
      sel (Cmp.Gt, translate_atom a1, translate_atom a2)
  | R_anf.(Prim (Ge (a1, a2), _)) ->
      sel (Cmp.Ge, translate_atom a1, translate_atom a2)
  | R_anf.(Let (x, e', e'', _)) ->
      do_select fn tails nv n e'' e1 e2 t
      |> explicate_assign fn tails nv n e' x
  | R_anf.(Begin (es, e, _)) ->
      List.fold_right es
        ~init:(do_select fn tails nv n e e1 e2 t)
        ~f:(explicate_effect fn tails nv n)
  | R_anf.(Prim (Vectorref _, t))
   |R_anf.(If (_, _, _, t))
   |R_anf.(Apply (_, _, t)) ->
      let x = fresh_var nv in
      sel (Cmp.Eq, Var (x, t), Bool true)
      |> explicate_assign fn tails nv n econd x
  | _ -> assert false

and explicate_effect fn tails nv n e cont =
  match e with
  | R_anf.Atom _ -> cont
  | R_anf.Prim (Read, _) -> Seq (Readstmt, cont)
  | R_anf.Prim (Print a, _) -> Seq (Printstmt (translate_atom a), cont)
  | R_anf.Prim (Vectorset (a1, i, a2), _) ->
      Seq (Vectorsetstmt (translate_atom a1, i, translate_atom a2), cont)
  | R_anf.Prim _ -> cont
  | R_anf.Let (v, e1, e2, _) ->
      let cont = explicate_effect fn tails nv n e2 cont in
      explicate_assign fn tails nv n e1 v cont
  | R_anf.If (e1, e2, e3, _) ->
      let l = fresh_label fn n in
      add_tail tails l cont;
      let tt = explicate_effect fn tails nv n e2 (Goto l) in
      let tf = explicate_effect fn tails nv n e3 (Goto l) in
      explicate_pred fn tails nv n e1 tt tf
  | R_anf.Apply (a, as', _) ->
      Seq (Callstmt (translate_atom a, List.map as' ~f:translate_atom), cont)
  | R_anf.Funref _ -> cont
  | R_anf.Setbang (v, e) -> explicate_assign fn tails nv n e v cont
  | R_anf.Begin (es, e, _) ->
      List.fold_right es
        ~init:(explicate_effect fn tails nv n e cont)
        ~f:(explicate_effect fn tails nv n)
  | R_anf.While (e1, e2) ->
      let l = fresh_label fn n in
      let loop = fresh_label fn n in
      add_tail tails l cont;
      let tt = explicate_effect fn tails nv n e2 (Goto loop) in
      let top = explicate_pred fn tails nv n e1 tt (Goto l) in
      add_tail tails loop top; Goto loop
  | R_anf.Collect n -> Seq (Collect n, cont)
  | R_anf.Allocate _ -> cont
  | R_anf.Globalvalue _ -> cont

and translate_atom = function
  | R_anf.Int i -> Int i
  | R_anf.Bool b -> Bool b
  | R_anf.Var (v, t) -> Var (v, t)
  | R_anf.Void -> Void

and translate_prim p =
  let tr = translate_atom in
  match p with
  | R_anf.Read -> Read
  | R_anf.Print _ -> assert false
  | R_anf.Minus a -> Minus (tr a)
  | R_anf.Plus (a1, a2) -> Plus (tr a1, tr a2)
  | R_anf.Subtract (a1, a2) -> Subtract (tr a1, tr a2)
  | R_anf.Mult (a1, a2) -> Mult (tr a1, tr a2)
  | R_anf.Div (a1, a2) -> Div (tr a1, tr a2)
  | R_anf.Rem (a1, a2) -> Rem (tr a1, tr a2)
  | R_anf.Land (a1, a2) -> Land (tr a1, tr a2)
  | R_anf.Lor (a1, a2) -> Lor (tr a1, tr a2)
  | R_anf.Lxor (a1, a2) -> Lxor (tr a1, tr a2)
  | R_anf.Lnot a -> Lnot (tr a)
  | R_anf.Eq (a1, a2) -> Eq (tr a1, tr a2)
  | R_anf.Lt (a1, a2) -> Lt (tr a1, tr a2)
  | R_anf.Le (a1, a2) -> Le (tr a1, tr a2)
  | R_anf.Gt (a1, a2) -> Gt (tr a1, tr a2)
  | R_anf.Ge (a1, a2) -> Ge (tr a1, tr a2)
  | R_anf.Not a -> Not (tr a)
  | R_anf.Vectorlength a -> Vectorlength (tr a)
  | R_anf.Vectorref (a, i) -> Vectorref (tr a, i)
  | R_anf.Vectorset _ -> assert false

and explicate_assign fn tails nv n e x cont =
  match e with
  | R_anf.(Let (v, e1, e2, _)) ->
      let cont = explicate_assign fn tails nv n e2 x cont in
      explicate_assign fn tails nv n e1 v cont
  | R_anf.(If (e1, e2, e3, _)) ->
      (* avoid duplicating code; do the assignments
       * and then go to the continuation *)
      let l = fresh_label fn n in
      add_tail tails l cont;
      let tt = explicate_assign fn tails nv n e2 x (Goto l) in
      let tf = explicate_assign fn tails nv n e3 x (Goto l) in
      explicate_pred fn tails nv n e1 tt tf
  | R_anf.(Begin (es, e, _)) ->
      List.fold_right es
        ~init:(explicate_assign fn tails nv n e x cont)
        ~f:(explicate_effect fn tails nv n)
  | R_anf.(While (e1, e2)) ->
      let l = fresh_label fn n in
      let loop = fresh_label fn n in
      add_tail tails l cont;
      let tt = explicate_effect fn tails nv n e2 (Goto loop) in
      let tf = Seq (Assign (x, Atom Void), Goto l) in
      let top = explicate_pred fn tails nv n e1 tt tf in
      add_tail tails loop top; Goto loop
  | _ ->
      let t = explicate_tail fn tails nv n e in
      do_assign fn tails nv n t x cont

and explicate_pred fn tails nv n cnd thn els =
  let tr = translate_atom in
  match cnd with
  | R_anf.(Atom (Bool b)) -> if b then thn else els
  | R_anf.(Atom (Var (x, t))) ->
      let lt = fresh_label fn n in
      let lf = fresh_label fn n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Eq, Var (x, t), Bool true), lt, lf)
  | R_anf.(Prim (Not a, _)) ->
      let lt = fresh_label fn n in
      let lf = fresh_label fn n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Eq, tr a, Bool false), lt, lf)
  | R_anf.(Prim (Eq (a1, a2), _)) ->
      let lt = fresh_label fn n in
      let lf = fresh_label fn n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Eq, tr a1, tr a2), lt, lf)
  | R_anf.(Prim (Lt (a1, a2), _)) ->
      let lt = fresh_label fn n in
      let lf = fresh_label fn n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Lt, tr a1, tr a2), lt, lf)
  | R_anf.(Prim (Le (a1, a2), _)) ->
      let lt = fresh_label fn n in
      let lf = fresh_label fn n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Le, tr a1, tr a2), lt, lf)
  | R_anf.(Prim (Gt (a1, a2), _)) ->
      let lt = fresh_label fn n in
      let lf = fresh_label fn n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Gt, tr a1, tr a2), lt, lf)
  | R_anf.(Prim (Ge (a1, a2), _)) ->
      let lt = fresh_label fn n in
      let lf = fresh_label fn n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Ge, tr a1, tr a2), lt, lf)
  | R_anf.(Prim (Vectorref _, t)) as r ->
      let x = fresh_var nv in
      let t =
        explicate_pred fn tails nv n R_anf.(Atom (Var (x, t))) thn els
      in
      explicate_assign fn tails nv n r x t
  | R_anf.(Let (x, e1, e2, _)) ->
      let t = explicate_pred fn tails nv n e2 thn els in
      explicate_assign fn tails nv n e1 x t
  | R_anf.(If (e1, e2, e3, _)) ->
      let lt = fresh_label fn n in
      let lf = fresh_label fn n in
      add_tail tails lt thn;
      add_tail tails lf els;
      let tt = explicate_pred fn tails nv n e2 (Goto lt) (Goto lf) in
      let tf = explicate_pred fn tails nv n e3 (Goto lt) (Goto lf) in
      explicate_pred fn tails nv n e1 tt tf
  | R_anf.Apply (_, _, t) as a ->
      (* this is a case where we're not in tail position for a call *)
      let x = fresh_var nv in
      let t =
        explicate_pred fn tails nv n R_anf.(Atom (Var (x, t))) thn els
      in
      explicate_assign fn tails nv n a x t
  | R_anf.Begin (es, e, _) ->
      List.fold_right es
        ~init:(explicate_pred fn tails nv n e thn els)
        ~f:(explicate_effect fn tails nv n)
  | _ -> assert false

and do_assign fn tails nv n t x cont =
  match t with
  | Return (Atom a) -> Seq (Assign (x, Atom a), cont)
  | Return (Prim (p, t)) -> Seq (Assign (x, Prim (p, t)), cont)
  | Return (Funref (v, t)) -> Seq (Assign (x, Funref (v, t)), cont)
  | Return (Call (a, as', t)) -> Seq (Assign (x, Call (a, as', t)), cont)
  | Return (Allocate (n, t)) -> Seq (Assign (x, Allocate (n, t)), cont)
  | Return (Globalvalue (v, t)) -> Seq (Assign (x, Globalvalue (v, t)), cont)
  | Return (Select (cmp, a1, a2, t)) ->
      Seq (Assign (x, Select (cmp, a1, a2, t)), cont)
  | Seq (s, t) -> Seq (s, do_assign fn tails nv n t x cont)
  | Tailcall (a, as', t) -> Seq (Assign (x, Call (a, as', t)), cont)
  | _ -> assert false

and fresh_label v n =
  let l = Printf.sprintf ".L%s%d" v !n in
  incr n; l

and fresh_var n =
  let v = Printf.sprintf "%%%d" !n in
  incr n; v

and add_tail tails l t = Hashtbl.set tails l t

let rec optimize_jumps = function
  | Program (info, defs) ->
      Program (info, List.map defs ~f:optimize_jumps_def)

and optimize_jumps_def = function
  | Def (v, args, t, info, tails) ->
      let cfg, tails = optimize_jumps_aux info.cfg tails in
      Def (v, args, t, {info with cfg}, tails)

and optimize_jumps_aux cfg tails =
  let singles =
    List.filter_map tails ~f:(fun (label, tail) ->
        match tail with
        | Goto label' -> Some (label, label')
        | _ -> None)
    |> Hashtbl.of_alist_exn (module Label)
  in
  let cfg, tails, changed =
    List.fold tails ~init:(cfg, [], false)
      ~f:(fun (cfg, tails, changed) (label, tail) ->
        if not (Cfg.mem_vertex cfg label) then (cfg, tails, true)
        else
          let cfg = ref cfg in
          let changed = ref changed in
          let rec aux = function
            | Return _ as r -> r
            | Seq (s, t) -> Seq (s, aux t)
            | Goto label' as g -> (
                if Label.equal label label' then g
                else
                  match Hashtbl.find singles label' with
                  | Some label'' when not (Label.equal label' label'') ->
                      cfg := Cfg.remove_vertex !cfg label';
                      cfg := Cfg.add_edge !cfg label label'';
                      changed := true;
                      Goto label''
                  | _ -> g )
            | If (cmp, lt, lf) as i ->
                if Label.(equal label lt || equal label lf) then i
                else
                  let lt' =
                    match Hashtbl.find singles lt with
                    | None -> lt
                    | Some lt' -> lt'
                  in
                  let lf' =
                    match Hashtbl.find singles lf with
                    | None -> lf
                    | Some lf' -> lf'
                  in
                  if not (Label.equal lt lt') then (
                    cfg := Cfg.remove_vertex !cfg lt;
                    cfg := Cfg.add_edge !cfg label lt';
                    changed := true );
                  if not (Label.equal lf lf') then (
                    cfg := Cfg.remove_vertex !cfg lf;
                    cfg := Cfg.add_edge !cfg label lf';
                    changed := true );
                  If (cmp, lt', lf')
            | Tailcall _ as t -> t
          in
          let tail = aux tail in
          (!cfg, (label, tail) :: tails, !changed))
  in
  let tails = List.rev tails in
  if changed then optimize_jumps_aux cfg tails else (cfg, tails)

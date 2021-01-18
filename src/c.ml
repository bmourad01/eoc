open Core_kernel
module Type = R_anf.Type

type type_env = R_anf.type_env

module Cfg = Graph.Persistent.Digraph.Concrete (Label)

type info = {main: Label.t; typ: Type.t; cfg: Cfg.t}

type var = R.var

module Cmp = struct
  type t = Eq | Lt | Le | Gt | Ge

  let to_string = function
    | Eq -> "eq?"
    | Lt -> "<"
    | Le -> "<="
    | Gt -> ">"
    | Ge -> ">="
end

type t = Program of info * tails

and tails = (Label.t * tail) list

and tail =
  | Return of exp
  | Seq of stmt * tail
  | Goto of Label.t
  | If of cmp * Label.t * Label.t

and stmt = Assign of var * exp | Collect of int

and exp =
  | Atom of atom
  | Prim of prim * Type.t
  | Allocate of int * Type.t
  | Globalvalue of string * Type.t

and atom = Int of int | Bool of bool | Var of var * Type.t | Void

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
  | Vectorset of atom * int * atom

and cmp = Cmp.t * atom * atom

let rec to_string = function
  | Program (_, tails) ->
      let tls =
        List.map tails ~f:(fun (l, t) ->
            Printf.sprintf "(%s:\n  %s)" l (string_of_tail t))
        |> String.concat ~sep:"\n "
      in
      Printf.sprintf "(%s)" tls

and string_of_tail = function
  | Return e -> Printf.sprintf "(return %s)" (string_of_exp e)
  | Seq (s, t) ->
      Printf.sprintf "%s\n  %s" (string_of_stmt s) (string_of_tail t)
  | Goto l -> Printf.sprintf "(goto %s)" l
  | If (cmp, lt, lf) ->
      Printf.sprintf "(if %s (goto %s) (goto %s))" (string_of_cmp cmp) lt lf

and string_of_stmt = function
  | Assign (v, e) -> Printf.sprintf "(set! %s %s)" v (string_of_exp e)
  | Collect n -> Printf.sprintf "(collect %d)" n

and string_of_exp = function
  | Atom a -> string_of_atom a
  | Prim (p, _) -> string_of_prim p
  | Allocate (n, t) -> Printf.sprintf "(allocate %d %s)" n (Type.to_string t)
  | Globalvalue (v, _) -> Printf.sprintf "(global-value '%s)" v

and string_of_atom = function
  | Int i -> Int.to_string i
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
  | Vectorset (a1, i, a2) ->
      Printf.sprintf "(vector-set! %s %d %s)" (string_of_atom a1) i
        (string_of_atom a2)

and string_of_cmp (cmp, a1, a2) =
  Printf.sprintf "(%s %s %s)" (Cmp.to_string cmp) (string_of_atom a1)
    (string_of_atom a2)

let read_int () =
  Out_channel.(flush stdout);
  Int.of_string In_channel.(input_line_exn stdin)

type answer = R_typed.answer

let rec interp ?(read = None) = function
  | Program (info, tails) -> (
      let tails = String.Map.of_alist_exn tails in
      match Map.find tails info.main with
      | None -> failwith "C.interp: no main label defined"
      | Some t -> interp_tail R_typed.empty_var_env tails t ~read )

and interp_tail ?(read = None) env tails = function
  | Return e -> interp_exp env e ~read
  | Seq (s, t) ->
      let env = interp_stmt env s ~read in
      interp_tail env tails t ~read
  | Goto l -> (
    match Map.find tails l with
    | None -> failwith ("C.interp: goto label " ^ l ^ " does not exist")
    | Some t -> interp_tail env tails t ~read )
  | If (cmp, lt, lf) -> (
    match interp_cmp env cmp with
    | `Bool b ->
        let t = if b then Goto lt else Goto lf in
        interp_tail env tails t ~read
    | _ -> assert false )

and interp_stmt ?(read = None) env = function
  | Assign (v, e) ->
      let e = interp_exp env e ~read in
      Map.set env v e
  | Collect _ -> env

and interp_exp ?(read = None) env = function
  | Atom a -> interp_atom env a
  | Prim (p, _) -> interp_prim env p ~read
  | Allocate (n, _) -> `Vector (Array.init n ~f:(fun _ -> `Void))
  | Globalvalue _ -> `Int 0

and interp_atom ?(read = None) env = function
  | Int i -> `Int i
  | Bool b -> `Bool b
  | Var (v, _) -> (
    match Map.find env v with
    | None -> failwith ("C.interp_atom: var " ^ v ^ " is unbound")
    | Some i -> i )
  | Void -> `Void

and interp_prim ?(read = None) env = function
  | Read -> (
    match read with
    | Some i -> `Int i
    | None -> `Int (read_int ()) )
  | Minus a -> (
    match interp_atom env a with
    | `Int i -> `Int (-i)
    | _ -> assert false )
  | Plus (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Int (i1 + i2)
    | _ -> assert false )
  | Subtract (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Int (i1 - i2)
    | _ -> assert false )
  | Mult (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Int (i1 * i2)
    | _ -> assert false )
  | Div (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Int (i1 / i2)
    | _ -> assert false )
  | Rem (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Int (i1 mod i2)
    | _ -> assert false )
  | Land (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Int (i1 land i2)
    | _ -> assert false )
  | Lor (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Int (i1 lor i2)
    | _ -> assert false )
  | Lxor (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Int (i1 lxor i2)
    | _ -> assert false )
  | Lnot a -> (
    match interp_atom env a with
    | `Int i -> `Int (lnot i)
    | _ -> assert false )
  | Eq (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Bool (i1 = i2)
    | `Bool b1, `Bool b2 -> `Bool (Bool.equal b1 b2)
    | _ -> assert false )
  | Lt (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Bool (i1 < i2)
    | _ -> assert false )
  | Le (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Bool (i1 <= i2)
    | _ -> assert false )
  | Gt (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Bool (i1 > i2)
    | _ -> assert false )
  | Ge (a1, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Int i1, `Int i2 -> `Bool (i1 >= i2)
    | _ -> assert false )
  | Not a -> (
    match interp_atom env a with
    | `Bool b -> `Bool (not b)
    | _ -> assert false )
  | Vectorlength a -> (
    match interp_atom env a with
    | `Vector v -> `Int (Array.length v)
    | _ -> assert false )
  | Vectorref (a, i) -> (
    match interp_atom env a with
    | `Vector v -> v.(i)
    | _ -> assert false )
  | Vectorset (a1, i, a2) -> (
    match (interp_atom env a1, interp_atom env a2) with
    | `Vector v, v' -> v.(i) <- v'; `Void
    | _ -> assert false )

and interp_cmp ?(read = None) env (cmp, a1, a2) =
  let e =
    match cmp with
    | Cmp.Eq -> Prim (Eq (a1, a2), Type.Boolean)
    | Cmp.Lt -> Prim (Lt (a1, a2), Type.Boolean)
    | Cmp.Le -> Prim (Le (a1, a2), Type.Boolean)
    | Cmp.Gt -> Prim (Gt (a1, a2), Type.Boolean)
    | Cmp.Ge -> Prim (Ge (a1, a2), Type.Boolean)
  in
  interp_exp env e ~read

let start_label = "main"

let rec explicate_control = function
  | R_anf.Program (info, e) ->
      let cfg = Cfg.(add_vertex empty start_label) in
      let info = {main= start_label; typ= info.typ; cfg} in
      (* we're not using a Hashtbl here because we 
       * want a specific ordering for each block *)
      let tails = ref Label.Map.empty in
      let tail = explicate_tail tails (ref 0) e in
      let tails = Map.set !tails start_label tail in
      let cfg =
        Map.fold tails ~init:cfg ~f:(fun ~key:label ~data:tail cfg ->
            let rec aux = function
              | Return _ -> cfg
              | Seq (_, t) -> aux t
              | Goto l -> Cfg.add_edge cfg label l
              | If (_, lt, lf) ->
                  Cfg.(add_edge (add_edge cfg label lt) label lf)
            in
            aux tail)
      in
      let tails = Map.to_alist tails |> List.rev in
      Program ({info with cfg}, tails)

and explicate_tail tails n = function
  | R_anf.(Atom a) -> Return (Atom (translate_atom a))
  | R_anf.(Prim (p, t)) -> Return (Prim (translate_prim p, t))
  | R_anf.(Let (v, e1, e2, _)) ->
      let cont = explicate_tail tails n e2 in
      explicate_assign tails n e1 v cont
  | R_anf.(If (e1, e2, e3, _)) ->
      let tt = explicate_tail tails n e2 in
      let tf = explicate_tail tails n e3 in
      explicate_pred tails n e1 tt tf
  | R_anf.Collect n -> Seq (Collect n, Return (Atom Void))
  | R_anf.Allocate (n, t) -> Return (Allocate (n, t))
  | R_anf.Globalvalue (v, t) -> Return (Globalvalue (v, t))

and translate_atom = function
  | R_anf.Int i -> Int i
  | R_anf.Bool b -> Bool b
  | R_anf.Var (v, t) -> Var (v, t)
  | R_anf.Void -> Void

and translate_prim p =
  let tr = translate_atom in
  match p with
  | R_anf.Read -> Read
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
  | R_anf.Vectorset (a1, i, a2) -> Vectorset (tr a1, i, tr a2)

and explicate_assign tails n e x cont =
  match e with
  | R_anf.(Let (v, e1, e2, _)) ->
      let cont = explicate_assign tails n e2 x cont in
      explicate_assign tails n e1 v cont
  | R_anf.(If (e1, e2, e3, _)) ->
      let tt = explicate_assign tails n e2 x cont in
      let tf = explicate_assign tails n e3 x cont in
      explicate_pred tails n e1 tt tf
  | _ ->
      let t = explicate_tail tails n e in
      do_assign t x cont

and explicate_pred tails n cnd thn els =
  let tr = translate_atom in
  match cnd with
  | R_anf.(Atom (Bool b)) -> if b then thn else els
  | R_anf.(Atom (Var (x, t))) ->
      let lt = fresh_label n in
      let lf = fresh_label n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Eq, Var (x, t), Bool true), lt, lf)
  | R_anf.(Prim (Not a, _)) ->
      let lt = fresh_label n in
      let lf = fresh_label n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Eq, tr a, Bool false), lt, lf)
  | R_anf.(Prim (Eq (a1, a2), _)) ->
      let lt = fresh_label n in
      let lf = fresh_label n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Eq, tr a1, tr a2), lt, lf)
  | R_anf.(Prim (Lt (a1, a2), _)) ->
      let lt = fresh_label n in
      let lf = fresh_label n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Lt, tr a1, tr a2), lt, lf)
  | R_anf.(Prim (Le (a1, a2), _)) ->
      let lt = fresh_label n in
      let lf = fresh_label n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Le, tr a1, tr a2), lt, lf)
  | R_anf.(Prim (Gt (a1, a2), _)) ->
      let lt = fresh_label n in
      let lf = fresh_label n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Gt, tr a1, tr a2), lt, lf)
  | R_anf.(Prim (Ge (a1, a2), _)) ->
      let lt = fresh_label n in
      let lf = fresh_label n in
      add_tail tails lt thn;
      add_tail tails lf els;
      If ((Cmp.Ge, tr a1, tr a2), lt, lf)
  | R_anf.(Let (x, e1, e2, _)) ->
      let t = explicate_pred tails n e2 thn els in
      explicate_assign tails n e1 x t
  | R_anf.(If (e1, e2, e3, _)) ->
      let lt = fresh_label n in
      let lf = fresh_label n in
      add_tail tails lt thn;
      add_tail tails lf els;
      let tt = explicate_pred tails n e2 (Goto lt) (Goto lf) in
      let tf = explicate_pred tails n e3 (Goto lt) (Goto lf) in
      explicate_pred tails n e1 tt tf
  | _ -> assert false

and do_assign e x cont =
  match e with
  | Return (Atom a) -> Seq (Assign (x, Atom a), cont)
  | Return (Prim (p, t)) -> Seq (Assign (x, Prim (p, t)), cont)
  | Return (Allocate (n, t)) -> Seq (Assign (x, Allocate (n, t)), cont)
  | Return (Globalvalue (v, t)) -> Seq (Assign (x, Globalvalue (v, t)), cont)
  | Seq (s, t) -> Seq (s, do_assign t x cont)
  | _ -> assert false

and fresh_label n =
  (* in the future, this will be the name of the current function *)
  let l = Printf.sprintf ".L%s%d" start_label !n in
  incr n; l

and add_tail tails l t = tails := Map.set !tails l t

let rec optimize_jumps = function
  | Program (info, tails) ->
      let cfg, tails = optimize_jumps_aux info.cfg tails in
      Program ({info with cfg}, tails)

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
          in
          let tail = aux tail in
          (!cfg, (label, tail) :: tails, !changed))
  in
  let tails = List.rev tails in
  if changed then optimize_jumps_aux cfg tails else (cfg, tails)

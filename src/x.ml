open Core_kernel

type label = string

type 'a label_map = 'a C.label_map

let empty_label_map = C.empty_label_map

let word_size = 8

module Reg = struct
  type t =
    | RSP
    | RBP
    | RAX
    | RBX
    | RCX
    | RDX
    | RSI
    | RDI
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
  [@@deriving equal, compare, hash, sexp]

  let to_string = function
    | RSP -> "rsp"
    | RBP -> "rbp"
    | RAX -> "rax"
    | RBX -> "rbx"
    | RCX -> "rcx"
    | RDX -> "rdx"
    | RSI -> "rsi"
    | RDI -> "rdi"
    | R8 -> "r8"
    | R9 -> "r9"
    | R10 -> "r10"
    | R11 -> "r11"
    | R12 -> "r12"
    | R13 -> "r13"
    | R14 -> "r14"
    | R15 -> "r15"

  let caller_save = [RAX; RCX; RDX; RSI; RDI; R8; R9; R10; R11]

  let callee_save = [RSP; RBP; RBX; R12; R13; R14; R15]

  let arg_passing = [RDI; RSI; RDX; RCX; R8; R9]

  let is_caller_save = List.mem caller_save ~equal

  let is_callee_save = List.mem callee_save ~equal

  let is_arg_passing = List.mem arg_passing ~equal
end

module Arg = struct
  module T = struct
    type t =
      | Imm of int
      | Reg of Reg.t
      | Deref of Reg.t * int
      | Var of R.var
    [@@deriving equal, compare, hash, sexp]

    let to_string = function
      | Imm i -> Int.to_string i
      | Reg r -> Reg.to_string r
      | Deref (r, i) ->
          if i = 0 then Printf.sprintf "qword [%s]" (Reg.to_string r)
          else if i < 0 then
            Printf.sprintf "qword [%s - %d]" (Reg.to_string r) (-i)
          else Printf.sprintf "qword [%s + %d]" (Reg.to_string r) i
      | Var v -> v
  end

  include T
  include Comparable.Make (T)
end

module Args = Set.Make (Arg)
module Arg_map = Map.Make (Arg)
module Interference_graph = Graph.Persistent.Graph.Concrete (Arg)

type info =
  { main: label
  ; locals_types: R.type_env
  ; stack_space: int
  ; conflicts: Interference_graph.t }

type t = Program of info * blocks

and blocks = block label_map

and block = Block of label * block_info * instr list

and block_info = {live_after: Args.t list}

and instr =
  | ADD of arg * arg
  | SUB of arg * arg
  | IMUL of arg * arg
  | IMULi of arg * arg * arg
  | NEG of arg
  | MOV of arg * arg
  | CALL of label * int
  | PUSH of arg
  | POP of arg
  | RET
  | JMP of label

and arg = Arg.t

let read_int = "read_int"

let print_int = "print_int"

let externs = [read_int; print_int]

let rec to_string = function
  | Program (info, blocks) ->
      let blks =
        Map.data blocks
        |> List.map ~f:string_of_block
        |> String.concat ~sep:"\n"
      in
      let externs =
        List.map externs ~f:(fun x -> "extern " ^ x)
        |> String.concat ~sep:"\n"
      in
      Printf.sprintf "global %s\n\n%s\n\nsection .text\n%s" info.main externs
        blks

and string_of_block = function
  | Block (l, _, is) ->
      Printf.sprintf "%s:\n    %s" l
        (List.map is ~f:string_of_instr |> String.concat ~sep:"\n    ")

and string_of_instr = function
  | ADD (a1, a2) ->
      Printf.sprintf "add %s, %s" (Arg.to_string a1) (Arg.to_string a2)
  | SUB (a1, a2) ->
      Printf.sprintf "sub %s, %s" (Arg.to_string a1) (Arg.to_string a2)
  | IMUL (a1, a2) ->
      Printf.sprintf "imul %s, %s" (Arg.to_string a1) (Arg.to_string a2)
  | IMULi (a1, a2, a3) ->
      Printf.sprintf "imul %s, %s, %s" (Arg.to_string a1) (Arg.to_string a2)
        (Arg.to_string a3)
  | NEG a -> Printf.sprintf "neg %s" (Arg.to_string a)
  | MOV (a1, a2) ->
      Printf.sprintf "mov %s, %s" (Arg.to_string a1) (Arg.to_string a2)
  | CALL (l, _) -> Printf.sprintf "call %s" l
  | PUSH a -> Printf.sprintf "push %s" (Arg.to_string a)
  | POP a -> Printf.sprintf "pop %s" (Arg.to_string a)
  | RET -> "ret"
  | JMP l -> Printf.sprintf "jmp %s" l

let fits_int32 i = Option.is_some (Int32.of_int i)

let rec select_instructions = function
  | C.Program (info, tails) ->
      let blocks =
        let block_info = {live_after= []} in
        Map.fold tails ~init:empty_label_map
          ~f:(fun ~key:label ~data:tail blocks ->
            let instrs = select_instructions_tail tail |> snd in
            Map.set blocks label (Block (label, block_info, instrs)))
      in
      let info =
        { main= info.main
        ; locals_types= info.locals_types
        ; stack_space= 0
        ; conflicts= Interference_graph.empty }
      in
      Program (info, blocks)

and select_instructions_tail t =
  let open Arg in
  match t with
  | C.Return (Atom (Int i)) ->
      let a = Reg RAX in
      (a, [MOV (a, Imm i); RET])
  | C.Return (Atom (Var v)) ->
      let a = Reg RAX in
      (a, [MOV (a, Var v); RET])
  | C.Return (Prim Read) ->
      let a = Reg RAX in
      (a, [CALL (read_int, 0); RET])
  | C.Return (Prim (Minus (Int i))) ->
      let a = Reg RAX in
      (a, [MOV (a, Imm (-i)); RET])
  | C.Return (Prim (Minus (Var v))) ->
      let a = Reg RAX in
      (a, [MOV (a, Var v); NEG a; RET])
  | C.Return (Prim (Plus (Int i1, Int i2))) ->
      let a = Reg RAX in
      (a, [MOV (a, Imm (i1 + i2)); RET])
  | C.Return (Prim (Subtract (Int i1, Int i2))) ->
      let a = Reg RAX in
      (a, [MOV (a, Imm (i1 - i2)); RET])
  | C.Return (Prim (Plus (Var v, Int i)))
   |C.Return (Prim (Plus (Int i, Var v))) ->
      let a = Reg RAX in
      (a, [MOV (a, Var v); ADD (a, Imm i); RET])
  | C.Return (Prim (Plus (Var v1, Var v2))) ->
      let a = Reg RAX in
      (a, [MOV (a, Var v1); ADD (a, Var v2); RET])
  | C.Return (Prim (Subtract (Var v, Int i))) ->
      let a = Reg RAX in
      (a, [MOV (a, Var v); SUB (a, Imm i)])
  | C.Return (Prim (Subtract (Int i, Var v))) ->
      let a = Reg RAX in
      (a, [MOV (a, Imm i); SUB (a, Var v); RET])
  | C.Return (Prim (Subtract (Var v1, Var v2))) ->
      let a = Reg RAX in
      (a, [MOV (a, Var v1); SUB (a, Var v2); RET])
  | C.Return (Prim (Mult (Int i1, Int i2))) ->
      let a = Reg RAX in
      (a, [MOV (a, Imm (i1 * i2))])
  | C.Return (Prim (Mult (Var v, Int i)))
   |C.Return (Prim (Mult (Int i, Var v))) ->
      let a = Reg RAX in
      if fits_int32 i then (a, [IMULi (a, Var v, Imm i)])
      else (a, [MOV (a, Imm i); IMUL (a, Var v)])
  | C.Return (Prim (Mult (Var v1, Var v2))) ->
      let a = Reg RAX in
      (a, [MOV (a, Var v1); IMUL (a, Var v2)])
  | C.Seq (s, t) ->
      let _, s = select_instructions_stmt s in
      let a, t = select_instructions_tail t in
      (a, s @ t)

and select_instructions_stmt s =
  let open Arg in
  match s with
  | C.Assign (v, Atom (Int i)) ->
      let a = Var v in
      (a, [MOV (a, Imm i)])
  | C.Assign (v, Atom (Var v')) ->
      let a = Var v in
      (a, [MOV (a, Var v')])
  | C.Assign (v, Prim Read) ->
      let a = Var v in
      (a, [CALL (read_int, 0); MOV (a, Reg RAX)])
  | C.Assign (v, Prim (Minus (Int i))) ->
      let a = Var v in
      (a, [MOV (a, Imm (-i))])
  | C.Assign (v, Prim (Minus (Var v'))) ->
      let a = Var v in
      if String.equal v v' then (a, [NEG a])
      else (a, [MOV (a, Var v'); NEG a])
  | C.Assign (v, Prim (Plus (Int i1, Int i2))) ->
      let a = Var v in
      (a, [MOV (a, Imm (i1 + i2))])
  | C.Assign (v, Prim (Plus (Var v', Int i)))
   |C.Assign (v, Prim (Plus (Int i, Var v'))) ->
      let a = Var v in
      if String.equal v v' then (a, [ADD (a, Imm i)])
      else (a, [MOV (a, Var v'); ADD (a, Imm i)])
  | C.Assign (v, Prim (Plus (Var v1, Var v2))) ->
      let a = Var v in
      (a, [MOV (a, Var v1); ADD (a, Var v2)])
  | C.Assign (v, Prim (Subtract (Int i1, Int i2))) ->
      let a = Var v in
      (a, [MOV (a, Imm (i1 - i2))])
  | C.Assign (v, Prim (Subtract (Var v', Int i))) ->
      let a = Var v in
      if String.equal v v' then (a, [SUB (a, Imm i)])
      else (a, [MOV (a, Var v'); SUB (a, Imm i)])
  | C.Assign (v, Prim (Subtract (Int i, Var v'))) ->
      let a = Var v in
      (a, [MOV (a, Imm i); SUB (a, Var v')])
  | C.Assign (v, Prim (Subtract (Var v1, Var v2))) ->
      let a = Var v in
      (a, [MOV (a, Var v1); SUB (a, Var v2)])
  | C.Assign (v, Prim (Mult (Int i1, Int i2))) ->
      let a = Var v in
      (a, [MOV (a, Imm (i1 * i2))])
  | C.Assign (v, Prim (Mult (Var v', Int i)))
   |C.Assign (v, Prim (Mult (Int i, Var v'))) ->
      let a = Var v in
      if fits_int32 i then (a, [IMULi (a, Var v', Imm i)])
      else (a, [MOV (a, Imm i); IMUL (a, Var v')])
  | C.Assign (v, Prim (Mult (Var v1, Var v2))) ->
      let a = Var v in
      (a, [MOV (a, Var v1); IMUL (a, Var v2)])

let is_temp_var_name = String.is_prefix ~prefix:"%"

let caller_save_set =
  List.map Reg.caller_save ~f:(fun r -> Arg.Reg r) |> Args.of_list

let rec assign_homes = function
  | Program (info, blocks) ->
      let stack_space =
        if Map.is_empty info.locals_types then 0
        else (Map.length info.locals_types * word_size lor 15) + 1
      in
      let blocks, _ =
        Map.fold blocks ~init:(empty_label_map, R.empty_var_env)
          ~f:(fun ~key:label ~data:block (blocks, env) ->
            let block, env = assign_homes_block env block in
            (Map.set blocks label block, env))
      in
      Program ({info with stack_space}, blocks)

and assign_homes_block env = function
  | Block (label, info, instrs) ->
      let instrs, env =
        List.fold instrs ~init:([], env) ~f:(fun (instrs, env) instr ->
            let instr, env = assign_homes_instr env instr in
            (instr :: instrs, env))
      in
      (Block (label, info, List.rev instrs), env)

and assign_homes_instr env = function
  | ADD (a1, a2) ->
      let a1, env = assign_homes_arg env a1 in
      let a2, env = assign_homes_arg env a2 in
      (ADD (a1, a2), env)
  | SUB (a1, a2) ->
      let a1, env = assign_homes_arg env a1 in
      let a2, env = assign_homes_arg env a2 in
      (SUB (a1, a2), env)
  | IMUL (a1, a2) ->
      let a1, env = assign_homes_arg env a1 in
      let a2, env = assign_homes_arg env a2 in
      (IMUL (a1, a2), env)
  | IMULi (a1, a2, a3) ->
      let a1, env = assign_homes_arg env a1 in
      let a2, env = assign_homes_arg env a2 in
      (IMULi (a1, a2, a3), env)
  | NEG a ->
      let a, env = assign_homes_arg env a in
      (NEG a, env)
  | MOV (a1, a2) ->
      let a1, env = assign_homes_arg env a1 in
      let a2, env = assign_homes_arg env a2 in
      (MOV (a1, a2), env)
  | CALL _ as c -> (c, env)
  | PUSH a ->
      let a, env = assign_homes_arg env a in
      (PUSH a, env)
  | POP a ->
      let a, env = assign_homes_arg env a in
      (POP a, env)
  | RET -> (RET, env)
  | JMP _ as j -> (j, env)

and assign_homes_arg env = function
  | Imm _ as i -> (i, env)
  | Reg _ as r -> (r, env)
  | Deref _ as d -> (d, env)
  | Var v when is_temp_var_name v -> (
    match Map.find env v with
    | None ->
        let offset =
          let inc = -word_size in
          Map.fold env ~init:inc ~f:(fun ~key:_ ~data:offset acc ->
              if offset <= acc then offset + inc else acc)
        in
        let env = Map.set env v offset in
        (Deref (RBP, offset), env)
    | Some offset -> (Deref (RBP, offset), env) )
  | Var _ as v -> (v, env)

let filter_non_locations =
  Set.filter ~f:(function
    | Arg.Imm _ -> false
    | _ -> true)

let write_set instr =
  let aux = function
    | ADD (a, _)
     |SUB (a, _)
     |NEG a
     |MOV (a, _)
     |IMUL (a, _)
     |IMULi (a, _, _) -> Args.singleton a
    | CALL _ -> Set.add caller_save_set (Reg RSP)
    | PUSH _ | RET -> Args.singleton (Reg RSP)
    | POP a -> Args.of_list [a; Reg RSP]
    | JMP _ -> Args.empty
  in
  aux instr |> filter_non_locations

let read_set instr =
  let aux = function
    | ADD (a1, a2) | SUB (a1, a2) | IMUL (a1, a2) -> Args.of_list [a1; a2]
    | NEG a | MOV (_, a) | IMULi (_, a, _) -> Args.singleton a
    | CALL (_, arity) ->
        List.take Reg.arg_passing arity
        |> List.map ~f:(fun r -> Arg.Reg r)
        |> List.append [Arg.Reg RSP]
        |> Args.of_list
    | PUSH a -> Args.of_list [a; Reg RSP]
    | POP _ | RET -> Args.singleton (Reg RSP)
    | JMP _ -> Args.empty
  in
  aux instr |> filter_non_locations

let function_prologue stack_space w =
  let setup_frame =
    if stack_space <= 0 then [] else [PUSH (Reg RBP); MOV (Reg RBP, Reg RSP)]
  in
  let callee_save_in_use =
    Set.fold w ~init:[] ~f:(fun acc a -> PUSH a :: acc) |> List.rev
  in
  let adj_sp =
    if stack_space <= 0 then [] else [SUB (Reg RSP, Imm stack_space)]
  in
  setup_frame @ callee_save_in_use @ adj_sp

let insert_function_epilogue label stack_space w instrs =
  let adj_sp =
    if stack_space <= 0 then [] else [ADD (Reg RSP, Imm stack_space)]
  in
  let callee_save_in_use =
    Set.fold w ~init:[] ~f:(fun acc a -> POP a :: acc)
  in
  let restore_frame = if stack_space <= 0 then [] else [POP (Reg RBP)] in
  let epilogue = adj_sp @ callee_save_in_use @ restore_frame in
  let rec aux acc = function
    | [] ->
        failwith
          ( "X.insert_function_epilogue: block " ^ label
          ^ " is not well-formed" )
    | RET :: _ ->
        List.rev acc
        @ [MOV (Reg RDI, Reg RAX); CALL (print_int, 1)]
        @ epilogue @ [RET]
    | instr :: rest -> aux (instr :: acc) rest
  in
  aux [] instrs

let rec patch_instructions = function
  | Program (info, blocks) ->
      let blocks = Map.map blocks ~f:(patch_instructions_block info) in
      Program (info, blocks)

and patch_instructions_block info = function
  | Block (label, block_info, instrs) ->
      let instrs =
        List.map instrs ~f:patch_instructions_instr |> List.concat
      in
      let instrs =
        if String.equal label info.main then
          let w =
            List.fold instrs ~init:Args.empty ~f:(fun w instr ->
                Set.union w (write_set instr)
                |> Set.filter ~f:(function
                     | Arg.Reg RSP -> false
                     | Arg.Reg RBP -> false
                     | Arg.Reg r -> Reg.is_callee_save r
                     | _ -> false))
          in
          function_prologue info.stack_space w
          @ insert_function_epilogue label info.stack_space w instrs
        else instrs
      in
      Block (label, block_info, instrs)

and patch_instructions_instr = function
  | ADD ((Deref _ as d1), (Deref _ as d2)) ->
      [MOV (Reg RAX, d2); ADD (d1, Reg RAX)]
  | SUB ((Deref _ as d1), (Deref _ as d2)) ->
      [MOV (Reg RAX, d2); SUB (d1, Reg RAX)]
  | MOV ((Deref _ as d1), (Deref _ as d2)) ->
      [MOV (Reg RAX, d2); MOV (d1, Reg RAX)]
  | MOV (a1, a2) when Arg.equal a1 a2 -> []
  | IMUL ((Deref _ as d), a) ->
      [MOV (Reg RAX, d); IMUL (Reg RAX, a); MOV (d, Reg RAX)]
  | IMULi ((Deref _ as d), a, i) ->
      [MOV (Reg RAX, d); IMULi (Reg RAX, a, i); MOV (d, Reg RAX)]
  | instr -> [instr]

let rec uncover_live = function
  | Program (info, blocks) ->
      let blocks = Map.map blocks ~f:uncover_live_block in
      Program (info, blocks)

and uncover_live_block = function
  | Block (label, info, instrs) ->
      let live_after, _ =
        List.fold_right instrs
          ~init:([], [Args.of_list [Reg RAX; Reg RSP]])
          ~f:(fun instr (live_after, live_before) ->
            let live_before' = List.hd_exn live_before in
            let live_after' = live_before' in
            let w = write_set instr in
            let r = read_set instr in
            let live_before'' = Set.(union (diff live_after' w) r) in
            (live_after' :: live_after, live_before'' :: live_before))
      in
      Block (label, {live_after}, instrs)

let rec build_interference = function
  | Program (info, blocks) ->
      let conflicts =
        let init = Interference_graph.empty in
        Map.fold blocks ~init ~f:(fun ~key:_ ~data:block g ->
            build_interference_block g block)
      in
      Program ({info with conflicts}, blocks)

and build_interference_block g = function
  | Block (_, info, instrs) ->
      List.zip_exn info.live_after instrs
      |> List.fold ~init:g ~f:(fun g (la, instr) ->
             let w = write_set instr in
             Set.fold la ~init:g ~f:(fun g v ->
                 match instr with
                 | MOV (d, s) ->
                     if Arg.(equal v d || equal v s) then g
                     else Interference_graph.add_edge g v d
                 | _ ->
                     Set.fold w ~init:g ~f:(fun g d ->
                         if Arg.equal v d then g
                         else Interference_graph.add_edge g v d)))

let allocatable_regs =
  (* prioritize caller-save registers over callee-save registers *)
  [| Arg.Reg RCX
   ; Arg.Reg RDX
   ; Arg.Reg RSI
   ; Arg.Reg RDI
   ; Arg.Reg R8
   ; Arg.Reg R9
   ; Arg.Reg R10
   ; Arg.Reg R11
   ; Arg.Reg RBX
   ; Arg.Reg R12
   ; Arg.Reg R13
   ; Arg.Reg R14
   ; Arg.Reg R15 |]

let color_graph g =
  let cmp (_, n) (_, m) = Int.compare m n in
  let q = Pairing_heap.create ~cmp () in
  Interference_graph.iter_vertex
    (fun v -> Pairing_heap.add q (v, Interference_graph.in_degree g v))
    g;
  let rec loop colors =
    match Pairing_heap.pop q with
    | None -> colors
    | Some (u, _) -> (
      match u with
      | Arg.Var _ ->
          let assigned =
            Interference_graph.fold_succ
              (fun v assigned ->
                match Map.find colors v with
                | None -> assigned
                | Some c -> Set.add assigned c)
              g u Int.Set.empty
          in
          let c = ref 0 in
          while Set.mem assigned !c do
            incr c
          done;
          loop (Map.set colors u !c)
      | _ -> loop colors )
  in
  (* registers which we will not select *)
  let colors =
    Interference_graph.fold_vertex
      (fun v colors ->
        match v with
        | Reg RAX -> Map.set colors v (-1)
        | Reg RSP -> Map.set colors v (-2)
        | Reg RBP -> Map.set colors v (-3)
        | _ -> colors)
      g Arg_map.empty
  in
  (* assign registers with their numbers *)
  let colors =
    Array.foldi allocatable_regs ~init:colors ~f:(fun i colors a ->
        if Interference_graph.mem_vertex g a then Map.set colors a i
        else colors)
  in
  loop colors

let rec allocate_registers = function
  | Program (info, blocks) ->
      let colors = color_graph info.conflicts in
      let blocks, vars =
        Map.fold blocks ~init:(empty_label_map, String.Set.empty)
          ~f:(fun ~key:label ~data:block (blocks, vars) ->
            let block, vars = allocate_registers_block colors vars block in
            (Map.set blocks label block, vars))
      in
      let locals_types = info.locals_types in
      let (Program (info, blocks)) =
        (* we can re-use assign-homes to spill variables onto the stack,
         * but this requires a bit of a hack, since assign-homes relies
         * on locals-types to create stack space. those variables which
         * were allocated to registers need to be removed from this map
         * when we run assign-homes, but we can restore it later. *)
        let locals_types =
          Map.filter_keys locals_types ~f:(fun v -> not (Set.mem vars v))
        in
        assign_homes (Program ({info with locals_types}, blocks))
      in
      Program ({info with locals_types}, blocks)

and allocate_registers_block colors vars = function
  | Block (label, info, instrs) ->
      let instrs, vars =
        List.fold instrs ~init:([], vars) ~f:(fun (instrs, vars) instr ->
            let instr, vars = allocate_registers_instr colors vars instr in
            (instr :: instrs, vars))
      in
      (Block (label, info, List.rev instrs), vars)

and allocate_registers_instr colors vars = function
  | ADD (a1, a2) ->
      let a1, vars = color_arg colors vars a1 in
      let a2, vars = color_arg colors vars a2 in
      (ADD (a1, a2), vars)
  | SUB (a1, a2) ->
      let a1, vars = color_arg colors vars a1 in
      let a2, vars = color_arg colors vars a2 in
      (SUB (a1, a2), vars)
  | IMUL (a1, a2) ->
      let a1, vars = color_arg colors vars a1 in
      let a2, vars = color_arg colors vars a2 in
      (IMUL (a1, a2), vars)
  | IMULi (a1, a2, a3) ->
      let a1, vars = color_arg colors vars a1 in
      let a2, vars = color_arg colors vars a2 in
      (IMULi (a1, a2, a3), vars)
  | NEG a ->
      let a, vars = color_arg colors vars a in
      (NEG a, vars)
  | MOV (a1, a2) ->
      let a1, vars = color_arg colors vars a1 in
      let a2, vars = color_arg colors vars a2 in
      (MOV (a1, a2), vars)
  | CALL _ as c -> (c, vars)
  | PUSH a ->
      let a, vars = color_arg colors vars a in
      (PUSH a, vars)
  | POP a ->
      let a, vars = color_arg colors vars a in
      (POP a, vars)
  | RET -> (RET, vars)
  | JMP _ as j -> (j, vars)

and color_arg colors vars = function
  | Arg.Var v as a when is_temp_var_name v -> (
    match Map.find colors a with
    | Some c when c >= 0 && c < Array.length allocatable_regs ->
        (allocatable_regs.(c), Set.add vars v)
    | _ -> (a, vars) )
  | a -> (a, vars)

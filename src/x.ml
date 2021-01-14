open Core_kernel

type label = string

type 'a label_map = 'a C.label_map

let empty_label_map = C.empty_label_map

let word_size = 8

type info = {main: label; locals_types: R.type_env; stack_space: int}

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
  [@@deriving equal, compare, sexp]

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
    [@@deriving equal, compare, sexp]

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

type t = Program of info * blocks

and blocks = block label_map

and block = Block of label * block_info * instr list

and block_info = {live_after: Args.t list}

and instr =
  | ADD of arg * arg
  | SUB of arg * arg
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
  | NEG a -> Printf.sprintf "neg %s" (Arg.to_string a)
  | MOV (a1, a2) ->
      Printf.sprintf "mov %s, %s" (Arg.to_string a1) (Arg.to_string a2)
  | CALL (l, _) -> Printf.sprintf "call %s" l
  | PUSH a -> Printf.sprintf "push %s" (Arg.to_string a)
  | POP a -> Printf.sprintf "pop %s" (Arg.to_string a)
  | RET -> "ret"
  | JMP l -> Printf.sprintf "jmp %s" l

let function_prologue stack_space =
  [ PUSH (Reg RBP)
  ; MOV (Reg RBP, Reg RSP)
  ; PUSH (Reg RBX)
  ; SUB (Reg RSP, Imm stack_space) ]

let insert_function_epilogue label stack_space instrs =
  let epilogue =
    [ADD (Reg RSP, Imm stack_space); POP (Reg RBX); POP (Reg RBP)]
  in
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

let rec select_instructions = function
  | C.Program (info, tails) ->
      let locals_types = info.locals_types in
      let stack_space = (Map.length locals_types * word_size lor 15) + 1 in
      let blocks =
        let block_info = {live_after= []} in
        Map.fold tails ~init:empty_label_map
          ~f:(fun ~key:label ~data:tail blocks ->
            let instrs = select_instructions_tail tail |> snd in
            let instrs =
              if
                String.equal label info.main
                && not (Map.is_empty locals_types)
              then
                function_prologue stack_space
                @ insert_function_epilogue label stack_space instrs
              else instrs
            in
            Map.set blocks label (Block (label, block_info, instrs)))
      in
      let info = {main= info.main; locals_types; stack_space} in
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
  | C.Return (Prim (Plus (Var v, Int i)))
   |C.Return (Prim (Plus (Int i, Var v))) ->
      let a = Reg RAX in
      (a, [MOV (a, Var v); ADD (a, Imm i); RET])
  | C.Return (Prim (Plus (Var v1, Var v2))) ->
      let a = Reg RAX in
      (a, [MOV (a, Var v1); ADD (a, Var v2); RET])
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

let is_temp_var_name = String.is_prefix ~prefix:"%"

let rec assign_homes = function
  | Program (info, blocks) ->
      let blocks, _ =
        Map.fold blocks ~init:(empty_label_map, R.empty_var_env)
          ~f:(fun ~key:label ~data:block (blocks, env) ->
            let block, env = assign_homes_block env block in
            (Map.set blocks label block, env))
      in
      Program (info, blocks)

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

let rec patch_instructions = function
  | Program (info, blocks) ->
      let blocks = Map.map blocks ~f:patch_instructions_block in
      Program (info, blocks)

and patch_instructions_block = function
  | Block (label, info, instrs) ->
      let instrs =
        List.map instrs ~f:patch_instructions_instr |> List.concat
      in
      Block (label, info, instrs)

and patch_instructions_instr = function
  | ADD ((Deref _ as d1), (Deref _ as d2)) ->
      [MOV (Reg RAX, d2); ADD (d1, Reg RAX)]
  | SUB ((Deref _ as d1), (Deref _ as d2)) ->
      [MOV (Reg RAX, d2); SUB (d1, Reg RAX)]
  | MOV ((Deref _ as d1), (Deref _ as d2)) ->
      [MOV (Reg RAX, d2); MOV (d1, Reg RAX)]
  | instr -> [instr]

let caller_save_set =
  List.map Reg.caller_save ~f:(fun r -> Arg.Reg r) |> Args.of_list

let write_set = function
  | ADD (a, _) | SUB (a, _) | NEG a | MOV (a, _) -> Args.singleton a
  | CALL _ -> Set.add caller_save_set (Reg RSP)
  | PUSH _ | RET -> Args.singleton (Reg RSP)
  | POP a -> Args.of_list [a; Reg RSP]
  | JMP _ -> Args.empty

let read_set = function
  | ADD (a1, a2) | SUB (a1, a2) -> Args.of_list [a1; a2]
  | NEG a | MOV (_, a) -> Args.singleton a
  | CALL (_, arity) ->
      List.take Reg.arg_passing arity
      |> List.map ~f:(fun r -> Arg.Reg r)
      |> List.append [Arg.Reg RSP]
      |> Args.of_list
  | PUSH a -> Args.of_list [a; Reg RSP]
  | POP _ | RET -> Args.singleton (Reg RSP)
  | JMP _ -> Args.empty

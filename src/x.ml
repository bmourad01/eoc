open Core_kernel

type label = string

type 'a label_map = 'a C.label_map

let empty_label_map = C.empty_label_map

let word_size = 8

type info = {main: label; locals_types: R.type_env; stack_space: int}

type t = Program of info * blocks

and blocks = block label_map

and block = Block of label * instr list

and instr =
  | ADD of arg * arg
  | SUB of arg * arg
  | NEG of arg
  | MOV of arg * arg
  | CALL of label
  | PUSH of arg
  | POP of arg
  | RET
  | JMP of label

and arg = Imm of int | Reg of reg | Deref of reg * int | Var of R.var

and reg =
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

let read_int = "read_int"

let externs = [read_int]

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
  | Block (l, is) ->
      Printf.sprintf "%s:\n    %s" l
        (List.map is ~f:string_of_instr |> String.concat ~sep:"\n    ")

and string_of_instr = function
  | ADD (a1, a2) ->
      Printf.sprintf "add %s, %s" (string_of_arg a1) (string_of_arg a2)
  | SUB (a1, a2) ->
      Printf.sprintf "sub %s, %s" (string_of_arg a1) (string_of_arg a2)
  | NEG a -> Printf.sprintf "neg %s" (string_of_arg a)
  | MOV (a1, a2) ->
      Printf.sprintf "mov %s, %s" (string_of_arg a1) (string_of_arg a2)
  | CALL l -> Printf.sprintf "call %s" l
  | PUSH a -> Printf.sprintf "push %s" (string_of_arg a)
  | POP a -> Printf.sprintf "pop %s" (string_of_arg a)
  | RET -> "ret"
  | JMP l -> Printf.sprintf "jmp %s" l

and string_of_arg = function
  | Imm i -> Int.to_string i
  | Reg r -> string_of_reg r
  | Deref (r, i) ->
      if i = 0 then Printf.sprintf "[%s]" (string_of_reg r)
      else if i < 0 then Printf.sprintf "[%s - %d]" (string_of_reg r) (-i)
      else Printf.sprintf "[%s + %d]" (string_of_reg r) i
  | Var v -> v

and string_of_reg = function
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

let rec select_instructions = function
  | C.Program (info, tails) ->
      let blocks =
        Map.fold tails ~init:empty_label_map
          ~f:(fun ~key:label ~data:tail blocks ->
            Map.set blocks label
              (Block (label, select_instructions_tail tail |> snd)))
      in
      let stack_space =
        (Map.length info.locals_types * word_size lor 15) + 1
      in
      let info =
        {main= info.main; locals_types= info.locals_types; stack_space}
      in
      Program (info, blocks)

and select_instructions_tail = function
  | C.Return (Atom (Int i)) ->
      let a = Reg RAX in
      (a, [MOV (a, Imm i); RET])
  | C.Return (Atom (Var v)) ->
      let a = Reg RAX in
      (a, [MOV (a, Var v); RET])
  | C.Return (Prim Read) ->
      let a = Reg RAX in
      (a, [CALL read_int; RET])
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

and select_instructions_stmt = function
  | C.Assign (v, Atom (Int i)) ->
      let a = Var v in
      (a, [MOV (a, Imm i)])
  | C.Assign (v, Atom (Var v')) ->
      let a = Var v in
      (a, [MOV (a, Var v')])
  | C.Assign (v, Prim Read) ->
      let a = Var v in
      (a, [CALL read_int; MOV (a, Reg RAX)])
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
  | Block (label, instrs) ->
      let instrs, env =
        List.fold instrs ~init:([], env) ~f:(fun (instrs, env) instr ->
            let instr, env = assign_homes_instr env instr in
            (instr :: instrs, env))
      in
      (Block (label, List.rev instrs), env)

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
          let inc = -8 in
          Map.fold env ~init:inc ~f:(fun ~key:_ ~data:offset acc ->
              if offset <= acc then offset + inc else acc)
        in
        let env = Map.set env v offset in
        (Deref (RBP, offset), env)
    | Some offset -> (Deref (RBP, offset), env) )
  | Var _ as v -> (v, env)

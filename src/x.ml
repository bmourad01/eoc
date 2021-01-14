open Core_kernel

type label = string

type info = {main: label}

type t = Program of info * blocks

and blocks = block String.Map.t

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

let rec to_string = function
  | Program (info, blocks) ->
      let blks =
        Map.data blocks
        |> List.map ~f:string_of_block
        |> String.concat ~sep:"\n"
      in
      Printf.sprintf "global %s\nextern %s\n\nsection .text\n%s" info.main
        read_int blks

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
  | Deref (r, i) -> Printf.sprintf "[%s + %d]" (string_of_reg r) i
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
        Map.fold tails ~init:String.Map.empty
          ~f:(fun ~key:label ~data:tail blocks ->
            Map.set blocks label
              (Block (label, select_instruction_tail tail |> snd)))
      in
      Program ({main= info.main}, blocks)

and select_instruction_tail = function
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
      let _, s = select_instruction_stmt s in
      let a, t = select_instruction_tail t in
      (a, s @ t)

and select_instruction_stmt = function
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
      (a, [MOV (a, Var v'); NEG a])
  | C.Assign (v, Prim (Plus (Int i1, Int i2))) ->
      let a = Var v in
      (a, [MOV (a, Imm (i1 + i2))])
  | C.Assign (v, Prim (Plus (Var v', Int i)))
   |C.Assign (v, Prim (Plus (Int i, Var v'))) ->
      let a = Var v in
      (a, [MOV (a, Var v'); ADD (a, Imm i)])
  | C.Assign (v, Prim (Plus (Var v1, Var v2))) ->
      let a = Var v in
      (a, [MOV (a, Var v1); ADD (a, Var v2)])

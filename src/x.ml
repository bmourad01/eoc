open Core_kernel

type info = unit

type label = string

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

and arg = Imm of int | Reg of reg | Deref of reg * int

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

let rec to_string = function
  | Program (_, blocks) ->
      let ls =
        Map.keys blocks
        |> List.map ~f:(fun l -> Printf.sprintf "global %s" l)
        |> String.concat ~sep:"\n"
      in
      let blks =
        Map.data blocks
        |> List.map ~f:string_of_block
        |> String.concat ~sep:"\n"
      in
      Printf.sprintf "%s\n%s\n" ls blks

and string_of_block = function
  | Block (l, is) ->
      Printf.sprintf "%s:\n    %s\n" l
        (List.map is ~f:string_of_instr |> String.concat ~sep:"    \n")

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

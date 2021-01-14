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

val to_string : t -> string

val string_of_block : block -> string

val string_of_instr : instr -> string

val string_of_arg : arg -> string

val string_of_reg : reg -> string

val select_instructions : C.t -> t

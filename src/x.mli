open Core_kernel

type label = string

type 'a label_map = 'a C.label_map

val empty_label_map : 'a label_map

val word_size : int

type info = {main: label; locals_types: R.type_env; stack_space: int}

module Reg : sig
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

  val to_string : t -> string
end

module Arg : sig
  type t = Imm of int | Reg of Reg.t | Deref of Reg.t * int | Var of R.var
  [@@deriving equal, compare, sexp]

  val to_string : t -> string
end

module Args : module type of Set.Make (Arg)

(* the X language, representing a subset of x86-64 programs.
 * X programs are printed in NASM syntax. *)

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

val to_string : t -> string

val string_of_block : block -> string

val string_of_instr : instr -> string

(* compile a C program to an X program with variables *)

val select_instructions : C.t -> t

(* allocate stack space for local variables *)

val assign_homes : t -> t

(* fix instructions where there is more than one memory operand *)

val patch_instructions : t -> t

(* uncover the live-after sets for each block *)

val uncover_live : t -> t

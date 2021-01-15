open Core_kernel

type label = string

type 'a label_map = 'a C.label_map

val empty_label_map : 'a label_map

val word_size : int

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
  [@@deriving equal, compare, hash, sexp]

  val to_string : t -> string
end

module Arg : sig
  type t = Imm of int | Reg of Reg.t | Deref of Reg.t * int | Var of R.var
  [@@deriving equal, compare, hash, sexp]

  val to_string : t -> string
end

module Args : module type of Set.Make (Arg)

module Arg_map : module type of Map.Make (Arg)

module Interference_graph :
    module type of Graph.Persistent.Graph.Concrete (Arg)

type info =
  { main: label
  ; locals_types: R.type_env
  ; stack_space: int
  ; conflicts: Interference_graph.t }

(* the X language, representing a subset of x86-64 programs.
 * X programs are printed in NASM syntax. *)

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

val to_string : t -> string

val string_of_block : block -> string

val string_of_instr : instr -> string

(* lower a C program to an X program with variables *)

val select_instructions : C.t -> t

(* spill local variables to the stack (deprecated) *)

val assign_homes : t -> t

(* fix instructions with illegal operands
 * + remove no-ops (e.g. MOV RAX, RAX)
 * + allocate stack space if necessary *)

val patch_instructions : t -> t

(* uncover the live-after sets for each block *)

val uncover_live : t -> t

(* build the interference graph based on the liveness information *)

val build_interference : t -> t

(* use the greedy saturation algorithm on the interference graph *)

val color_graph : Interference_graph.t -> int Arg_map.t

(* perform register allocation *)

val allocate_registers : t -> t

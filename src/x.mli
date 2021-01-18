open Core_kernel

val word_size : int

module Cc : sig
  type t = E | NE | L | LE | G | GE

  val to_string : t -> string
end

module Bytereg : sig
  (* the upper 8-bit registers cannot be used
   * in 64-bit mode when a REX prefix is present *)
  type t = AL | BL | CL | DL [@@deriving equal, compare, hash, sexp]

  val to_string : t -> string
end

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
  type t =
    | Imm of int
    | Reg of Reg.t
    | Bytereg of Bytereg.t
    | Deref of Reg.t * int
    | Var of R.var
  [@@deriving equal, compare, hash, sexp]

  val to_string : t -> string
end

module Args : module type of Set.Make (Arg)

module Arg_map : module type of Map.Make (Arg)

module Interference_graph :
    module type of Graph.Persistent.Graph.Concrete (Arg)

module Cfg : module type of C.Cfg

type info =
  { main: Label.t
  ; stack_space: int
  ; conflicts: Interference_graph.t
  ; typ: C.Type.t
  ; cfg: Cfg.t
  ; locals_types: C.type_env }

(* the X language, representing a subset of x86-64 programs.
 * X programs are printed in NASM syntax. *)

type t = Program of info * blocks

and blocks = (Label.t * block) list

and block = Block of Label.t * block_info * instr list

and block_info = {live_after: Args.t list}

and instr =
  | ADD of arg * arg
  | SUB of arg * arg
  | IMUL of arg * arg
  | IMULi of arg * arg * arg
  | IDIV of arg
  | NEG of arg
  | MOV of arg * arg
  | CALL of Label.t * int
  | PUSH of arg
  | POP of arg
  | RET
  | JMP of Label.t
  | NOT of arg
  | XOR of arg * arg
  | AND of arg * arg
  | OR of arg * arg
  | CMP of arg * arg
  | TEST of arg * arg
  | SETCC of Cc.t * arg
  | MOVZX of arg * arg
  | JCC of Cc.t * Label.t

and arg = Arg.t

val to_string : t -> string

val string_of_block : block -> string

val string_of_instr : instr -> string

(* lower a C program to an X program with variables *)

val select_instructions : C.t -> t

(* fix instructions with illegal operands
 * + remove no-ops (e.g. MOV RAX, RAX)
 * + allocate stack space if necessary *)

val patch_instructions : t -> t

(* uncover the live-after sets for each block *)

val uncover_live : t -> t

(* build the interference graph based on the liveness information *)

val build_interference : t -> t

(* use the greedy saturation algorithm on the interference graph *)

val color_graph :
  ?bias:Interference_graph.t -> Interference_graph.t -> int Arg_map.t

(* perform register allocation *)

val allocate_registers : t -> t

(* merge sequentially-adjacent blocks wherever possible *)

val remove_jumps : t -> t

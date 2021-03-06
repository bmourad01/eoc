open Core_kernel

val word_size : int

module Cc : sig
  type t = E | NE | L | LE | G | GE | B | BE | A | AE

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

module Xmmreg : sig
  type t =
    | XMM0
    | XMM1
    | XMM2
    | XMM3
    | XMM4
    | XMM5
    | XMM6
    | XMM7
    | XMM8
    | XMM9
    | XMM10
    | XMM11
    | XMM12
    | XMM13
    | XMM14
    | XMM15
  [@@deriving equal, compare, hash, sexp]

  val to_string : t -> string
end

module Arg : sig
  type t =
    | Imm of Int64.t
    | Reg of Reg.t
    | Xmmreg of Xmmreg.t
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

type info = {type_map: Label.t C.Type_map.t; float_map: Label.t Float.Map.t}

module Cfg : module type of C.Cfg

(* the X language, representing a subset of x86-64 programs.
 * X programs are printed in NASM syntax. *)

type t = Program of info * def list

and def = Def of def_info * Label.t * blocks

and def_info =
  { main: Label.t
  ; stack_space: int
  ; conflicts: Interference_graph.t
  ; cfg: Cfg.t
  ; typ: C.Type.t
  ; locals_types: C.type_env
  ; rootstack_spills: int
  ; floatstack_spills: int }

and blocks = (Label.t * block) list

and block = Block of Label.t * block_info * instr list

and block_info = {live_after: Args.t list}

and instr =
  | ADD of arg * arg
  | ADDSD of arg * arg
  | INC of arg
  | DEC of arg
  | SUB of arg * arg
  | SUBSD of arg * arg
  | IMUL of arg * arg
  | IMULi of arg * arg * arg
  | MULSD of arg * arg
  | XORPD of arg * arg
  | IDIV of arg
  | DIVSD of arg * arg
  | SQRTSD of arg * arg
  | NEG of arg
  | MOV of arg * arg
  | MOVSD of arg * arg
  | LEA of arg * arg
  | CALL of Label.t * int
  | CALLi of arg * int
  | PUSH of arg
  | POP of arg
  | RET
  | JMP of Label.t
  | JMPt of arg * int
  | NOT of arg
  | XOR of arg * arg
  | AND of arg * arg
  | OR of arg * arg
  | CMP of arg * arg
  | COMISD of arg * arg
  | TEST of arg * arg
  | SETCC of Cc.t * arg
  | CMOV of Cc.t * arg * arg
  | MOVZX of arg * arg
  | JCC of Cc.t * Label.t
  | MOVQ of arg * arg
  | CVTSI2SD of arg * arg
  | CVTSD2SI of arg * arg
  | BT of arg * arg

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
     ?bias:Interference_graph.t
  -> Interference_graph.t
  -> C.type_env
  -> int Arg_map.t

(* perform register allocation *)

val allocate_registers : t -> t

(* merge sequentially-adjacent blocks wherever possible *)

val remove_jumps : t -> t

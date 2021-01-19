open Core_kernel

let word_size = R_alloc.word_size

let total_tag_offset = R_alloc.total_tag_offset

let tag_offset = 0

let int_mask_offset = 1

let bool_mask_offset = 2

let void_mask_offset = 3

module Cc = struct
  type t = E | NE | L | LE | G | GE

  let to_string = function
    | E -> "e"
    | NE -> "ne"
    | L -> "l"
    | LE -> "le"
    | G -> "g"
    | GE -> "ge"

  let of_c_cmp = function
    | C.Cmp.Eq -> E
    | C.Cmp.Lt -> L
    | C.Cmp.Le -> LE
    | C.Cmp.Gt -> G
    | C.Cmp.Ge -> GE

  let of_c_cmp_swap = function
    | C.Cmp.Eq -> E
    | C.Cmp.Lt -> G
    | C.Cmp.Le -> GE
    | C.Cmp.Gt -> L
    | C.Cmp.Ge -> LE
end

module Bytereg = struct
  type t = AL | BL | CL | DL [@@deriving equal, compare, hash, sexp]

  let to_string = function
    | AL -> "al"
    | BL -> "bl"
    | CL -> "cl"
    | DL -> "dl"
end

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

let is_temp_var_name = String.is_prefix ~prefix:"%"

module Arg = struct
  module T = struct
    type t =
      | Imm of int
      | Reg of Reg.t
      | Bytereg of Bytereg.t
      | Deref of Reg.t * int
      | Var of R.var
    [@@deriving equal, compare, hash, sexp]

    let to_string = function
      | Imm i -> Int.to_string i
      | Reg r -> Reg.to_string r
      | Bytereg r -> Bytereg.to_string r
      | Deref (r, i) ->
          if i = 0 then Printf.sprintf "qword [%s]" (Reg.to_string r)
          else if i < 0 then
            Printf.sprintf "qword [%s - %d]" (Reg.to_string r) (-i)
          else Printf.sprintf "qword [%s + %d]" (Reg.to_string r) i
      | Var v when is_temp_var_name v -> v
      | Var v -> Printf.sprintf "qword [%s]" v
  end

  include T
  include Comparable.Make (T)
end

module Args = Set.Make (Arg)
module Arg_map = Map.Make (Arg)
module Interference_graph = Graph.Persistent.Graph.Concrete (Arg)
module Cfg = C.Cfg

type info =
  { main: Label.t
  ; stack_space: int
  ; conflicts: Interference_graph.t
  ; typ: C.Type.t
  ; cfg: Cfg.t
  ; locals_types: C.type_env
  ; rootstack_spills: int }

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

let filter_non_locations =
  Set.filter ~f:(function
    | Arg.Imm _ -> false
    | _ -> true)

let caller_save_set =
  List.map Reg.caller_save ~f:(fun r -> Arg.Reg r) |> Args.of_list

let callee_save_set =
  List.map Reg.callee_save ~f:(fun r -> Arg.Reg r) |> Args.of_list

let convert_bytereg = function
  | Arg.Bytereg AL -> Arg.Reg RAX
  | Arg.Bytereg BL -> Arg.Reg RBX
  | Arg.Bytereg CL -> Arg.Reg RCX
  | Arg.Bytereg DL -> Arg.Reg RDX
  | a -> a

let write_set instr =
  let aux = function
    | ADD (a, _)
     |SUB (a, _)
     |NEG a
     |MOV (a, _)
     |IMUL (a, _)
     |IMULi (a, _, _)
     |NOT a
     |XOR (a, _)
     |AND (a, _)
     |OR (a, _)
     |SETCC (_, a)
     |MOVZX (a, _) -> Args.singleton a
    | IDIV _ -> Args.of_list [Reg RAX; Reg RDX]
    | CALL _ -> Set.add caller_save_set (Reg RSP)
    | PUSH _ | RET -> Args.singleton (Reg RSP)
    | POP a -> Args.of_list [a; Reg RSP]
    | JMP _ | JCC _ | CMP _ | TEST _ -> Args.empty
  in
  aux instr |> filter_non_locations |> Args.map ~f:convert_bytereg

let read_set instr =
  let aux = function
    | XOR (a1, a2) when Arg.equal a1 a2 ->
        (* special case: it DOES read the source register
         * in order to compute the result, but in effect
         * it's just zeroing the destination, so we should
         * treat it as if it's not actually reading anything. *)
        Args.empty
    | ADD (a1, a2)
     |SUB (a1, a2)
     |IMUL (a1, a2)
     |XOR (a1, a2)
     |AND (a1, a2)
     |OR (a1, a2)
     |CMP (a1, a2)
     |TEST (a1, a2) -> Args.of_list [a1; a2]
    | IDIV a -> Args.of_list [a; Reg RAX; Reg RDX]
    | NEG a | MOV (_, a) | IMULi (_, a, _) | NOT a | MOVZX (_, a) ->
        Args.singleton a
    | CALL (_, arity) ->
        List.take Reg.arg_passing arity
        |> List.map ~f:(fun r -> Arg.Reg r)
        |> List.append [Arg.Reg RSP]
        |> Args.of_list
    | PUSH a -> Args.of_list [a; Reg RSP]
    | POP _ | RET -> Args.singleton (Reg RSP)
    | JMP _ | SETCC _ | JCC _ -> Args.empty
  in
  aux instr |> filter_non_locations |> Args.map ~f:convert_bytereg

module Extern = struct
  let read_int = "_read_int"

  let print_int = "_print_int"

  let print_bool = "_print_bool"

  let print_void = "_print_void"

  let print_vector = "_print_vector"

  let initialize = "_initialize"

  let collect = "_collect"

  let extern_fns =
    [ read_int
    ; print_int
    ; print_bool
    ; print_void
    ; print_vector
    ; initialize
    ; collect ]

  let free_ptr = R_alloc.free_ptr

  let fromspace_end = R_alloc.fromspace_end

  let rootstack_begin = "_rootstack_begin"

  let extern_vars = [free_ptr; fromspace_end; rootstack_begin]
end

let rec to_string = function
  | Program (info, blocks) ->
      let blks =
        List.map blocks ~f:snd
        |> List.map ~f:string_of_block
        |> String.concat ~sep:"\n"
      in
      let extern_fns =
        List.map Extern.extern_fns ~f:(fun x -> "extern " ^ x)
        |> String.concat ~sep:"\n"
      in
      let extern_vars =
        List.map Extern.extern_vars ~f:(fun x -> "extern " ^ x)
        |> String.concat ~sep:"\n"
      in
      Printf.sprintf
        "DEFAULT REL\n\nglobal %s\n\n%s\n\n%s\n\nsection .text\n%s" info.main
        extern_fns extern_vars blks

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
  | IDIV a -> Printf.sprintf "idiv %s" (Arg.to_string a)
  | NEG a -> Printf.sprintf "neg %s" (Arg.to_string a)
  | MOV (a1, a2) ->
      Printf.sprintf "mov %s, %s" (Arg.to_string a1) (Arg.to_string a2)
  | CALL (l, _) -> Printf.sprintf "call %s" l
  | PUSH a -> Printf.sprintf "push %s" (Arg.to_string a)
  | POP a -> Printf.sprintf "pop %s" (Arg.to_string a)
  | RET -> "ret"
  | JMP l -> Printf.sprintf "jmp %s" l
  | NOT a -> Printf.sprintf "not %s" (Arg.to_string a)
  | XOR (a1, a2) ->
      Printf.sprintf "xor %s, %s" (Arg.to_string a1) (Arg.to_string a2)
  | AND (a1, a2) ->
      Printf.sprintf "and %s, %s" (Arg.to_string a1) (Arg.to_string a2)
  | OR (a1, a2) ->
      Printf.sprintf "or %s, %s" (Arg.to_string a1) (Arg.to_string a2)
  | CMP (a1, a2) ->
      Printf.sprintf "cmp %s, %s" (Arg.to_string a1) (Arg.to_string a2)
  | TEST (a1, a2) ->
      Printf.sprintf "test %s, %s" (Arg.to_string a1) (Arg.to_string a2)
  | SETCC (cc, a) ->
      Printf.sprintf "set%s %s" (Cc.to_string cc) (Arg.to_string a)
  | MOVZX (a1, a2) ->
      Printf.sprintf "movzx %s, %s" (Arg.to_string a1) (Arg.to_string a2)
  | JCC (cc, l) -> Printf.sprintf "j%s %s" (Cc.to_string cc) l

let fits_int32 i = Option.is_some (Int32.of_int i)

let rec select_instructions = function
  | C.Program (info, tails) ->
      let blocks =
        let block_info = {live_after= []} in
        List.fold_right tails ~init:[] ~f:(fun (label, tail) blocks ->
            let instrs = select_instructions_tail tails tail in
            (label, Block (label, block_info, instrs)) :: blocks)
      in
      let info =
        { main= info.main
        ; stack_space= 0
        ; conflicts= Interference_graph.empty
        ; typ= info.typ
        ; cfg= info.cfg
        ; locals_types= info.locals_types
        ; rootstack_spills= 0 }
      in
      Program (info, blocks)

and select_instructions_tail tails t =
  let open Arg in
  match t with
  | C.Return e -> select_instructions_exp (Reg RAX) e @ [RET]
  | C.Seq (s, t) ->
      let s = select_instructions_stmt s in
      let t = select_instructions_tail tails t in
      s @ t
  (* goto *)
  | C.Goto l -> [JMP l]
  (* if *)
  | C.If ((cmp, Int i1, Int i2), lt, lf) -> (
    match cmp with
    | Eq -> if Int.(i1 = i2) then [JMP lt] else [JMP lf]
    | Lt -> if Int.(i1 < i2) then [JMP lt] else [JMP lf]
    | Le -> if Int.(i1 <= i2) then [JMP lt] else [JMP lf]
    | Gt -> if Int.(i1 > i2) then [JMP lt] else [JMP lf]
    | Ge -> if Int.(i1 >= i2) then [JMP lt] else [JMP lf] )
  | C.If ((cmp, Bool b1, Bool b2), lt, lf) -> (
    match cmp with
    | Eq -> if Bool.equal b1 b2 then [JMP lt] else [JMP lf]
    | _ -> assert false )
  | C.If ((cmp, Var (v, _), Int i), lt, lf) ->
      let cc = Cc.of_c_cmp cmp in
      [CMP (Var v, Imm i); JCC (cc, lt); JMP lf]
  | C.If ((cmp, Int i, Var (v, _)), lt, lf) ->
      let cc = Cc.of_c_cmp_swap cmp in
      [CMP (Var v, Imm i); JCC (cc, lt); JMP lf]
  | C.If ((cmp, Var (v, _), Bool b), lt, lf)
   |C.If ((cmp, Bool b, Var (v, _)), lt, lf) -> (
    match cmp with
    | Eq ->
        let lt, lf = if not b then (lt, lf) else (lf, lt) in
        [TEST (Var v, Var v); JCC (Cc.E, lt); JMP lf]
    | _ -> assert false )
  | C.If ((cmp, Var (v1, _), Var (v2, _)), lt, lf) when String.equal v1 v2
    -> (
    match cmp with
    | Eq -> [JMP lt]
    | _ -> [JMP lf] )
  | C.If ((cmp, Var (v1, _), Var (v2, _)), lt, lf) ->
      let cc = Cc.of_c_cmp cmp in
      [CMP (Var v1, Var v2); JCC (cc, lt); JMP lf]
  | C.If _ -> assert false

and select_instructions_stmt s =
  let open Arg in
  match s with
  | C.Assign (v, e) -> select_instructions_exp (Var v) e
  | C.Collect n ->
      [MOV (Reg RDI, Reg R15); MOV (Reg RSI, Imm n); CALL (Extern.collect, 2)]

and select_instructions_exp a p =
  let open Arg in
  match p with
  (* atom *)
  | C.(Atom (Int i)) -> if Int.(i = 0) then [XOR (a, a)] else [MOV (a, Imm i)]
  | C.(Atom (Bool b)) -> if not b then [XOR (a, a)] else [MOV (a, Imm 1)]
  | C.(Atom (Var (v, _))) -> [MOV (a, Var v)]
  | C.(Atom Void) -> [XOR (a, a)]
  (* read *)
  | C.(Prim (Read, _)) -> (
      let c = CALL (Extern.read_int, 0) in
      match a with
      | Arg.Reg RAX -> [c]
      | _ -> [c; MOV (a, Reg RAX)] )
  (* minus *)
  | C.(Prim (Minus (Int i), _)) -> [MOV (a, Imm (-i))]
  | C.(Prim (Minus (Var (v, t)), _)) -> [MOV (a, Var v); NEG a]
  | C.(Prim (Minus _, _)) -> assert false
  (* plus *)
  | C.(Prim (Plus (Int i1, Int i2), _)) ->
      let i = i1 + i2 in
      if Int.(i = 0) then [XOR (a, a)] else [MOV (a, Imm i)]
  | C.(Prim (Plus (Var (v, t), Int i), _))
   |C.(Prim (Plus (Int i, Var (v, t)), _)) -> [MOV (a, Var v); ADD (a, Imm i)]
  | C.(Prim (Plus (Var (v1, _), Var (v2, _)), _)) ->
      [MOV (a, Var v1); ADD (a, Var v2)]
  | C.(Prim (Plus _, _)) -> assert false
  (* subtract *)
  | C.(Prim (Subtract (Int i1, Int i2), _)) ->
      let i = i1 - i2 in
      if Int.(i = 0) then [XOR (a, a)] else [MOV (a, Imm (i1 - i2))]
  | C.(Prim (Subtract (Var (v, _), Int i), _)) ->
      [MOV (a, Var v); SUB (a, Imm i)]
  | C.(Prim (Subtract (Int i, Var (v, _)), _)) ->
      [MOV (a, Imm i); SUB (a, Var v)]
  | C.(Prim (Subtract (Var (v1, _), Var (v2, _)), _)) ->
      [MOV (a, Var v1); SUB (a, Var v2)]
  | C.(Prim (Subtract _, _)) -> assert false
  (* mult *)
  | C.(Prim (Mult (_, Int 0), _)) | C.(Prim (Mult (Int 0, _), _)) ->
      [XOR (a, a)]
  | C.(Prim (Mult (Int i1, Int i2), _)) ->
      let i = i1 * i2 in
      if Int.(i = 0) then [XOR (a, a)] else [MOV (a, Imm i)]
  | C.(Prim (Mult (Var (v, _), Int i), _))
   |C.(Prim (Mult (Int i, Var (v, _)), _)) ->
      if fits_int32 i then [IMULi (a, Var v, Imm i)]
      else [MOV (a, Imm i); IMUL (a, Var v)]
  | C.(Prim (Mult (Var (v1, _), Var (v2, _)), _)) ->
      [MOV (a, Var v1); IMUL (a, Var v2)]
  | C.(Prim (Mult _, _)) -> assert false
  (* div *)
  | C.(Prim (Div (Int 0, _), _)) -> [XOR (a, a)]
  | C.(Prim (Div (Int i1, Int i2), _)) ->
      let i = i1 / i2 in
      if Int.(i = 0) then [XOR (a, a)] else [MOV (a, Imm i)]
  | C.(Prim (Div (Var (v, _), Int i), _)) ->
      [ XOR (Reg RDX, Reg RDX)
      ; MOV (Reg RAX, Var v)
      ; MOV (Reg RCX, Imm i)
      ; IDIV (Reg RCX)
      ; MOV (a, Reg RAX) ]
  | C.(Prim (Div (Int i, Var (v, _)), _)) ->
      [ XOR (Reg RDX, Reg RDX)
      ; MOV (Reg RAX, Imm i)
      ; IDIV (Var v)
      ; MOV (a, Reg RAX) ]
  | C.(Prim (Div (Var (v1, _), Var (v2, _)), _)) ->
      [ XOR (Reg RDX, Reg RDX)
      ; MOV (Reg RAX, Var v1)
      ; IDIV (Var v2)
      ; MOV (a, Reg RAX) ]
  | C.(Prim (Div _, _)) -> assert false
  (* rem *)
  | C.(Prim (Rem (Int 0, _), _)) -> [XOR (a, a)]
  | C.(Prim (Rem (Int i1, Int i2), _)) ->
      let i = i1 mod i2 in
      if Int.(i = 0) then [XOR (a, a)] else [MOV (a, Imm i)]
  | C.(Prim (Rem (Var (v, _), Int i), _)) ->
      [ XOR (Reg RDX, Reg RDX)
      ; MOV (Reg RAX, Var v)
      ; MOV (Reg RCX, Imm i)
      ; IDIV (Reg RCX)
      ; MOV (a, Reg RDX) ]
  | C.(Prim (Rem (Int i, Var (v, _)), _)) ->
      [ XOR (Reg RDX, Reg RDX)
      ; MOV (Reg RAX, Imm i)
      ; IDIV (Var v)
      ; MOV (a, Reg RDX) ]
  | C.(Prim (Rem (Var (v1, _), Var (v2, _)), _)) ->
      [ XOR (Reg RDX, Reg RDX)
      ; MOV (Reg RAX, Var v1)
      ; IDIV (Var v2)
      ; MOV (a, Reg RDX) ]
  | C.(Prim (Rem _, _)) -> assert false
  (* land *)
  | C.(Prim (Land (Int 0, _), _)) | C.(Prim (Land (_, Int 0), _)) ->
      [XOR (a, a)]
  | C.(Prim (Land (Int i1, Int i2), _)) ->
      let i = i1 land i2 in
      if Int.(i = 0) then [XOR (a, a)] else [MOV (a, Imm i)]
  | C.(Prim (Land (Var (v, _), Int i), _))
   |C.(Prim (Land (Int i, Var (v, _)), _)) -> [MOV (a, Var v); AND (a, Imm i)]
  | C.(Prim (Land (Var (v1, _), Var (v2, _)), _)) when String.equal v1 v2 ->
      [MOV (a, Var v1)]
  | C.(Prim (Land (Var (v1, _), Var (v2, _)), _)) ->
      [MOV (a, Var v1); AND (a, Var v2)]
  | C.(Prim (Land _, _)) -> assert false
  (* lor *)
  | C.(Prim (Lor (Int i1, Int i2), _)) ->
      let i = i1 land i2 in
      if Int.(i = 0) then [XOR (a, a)] else [MOV (a, Imm (i1 lor i2))]
  | C.(Prim (Lor (Var (v, _), Int 0), _))
   |C.(Prim (Lor (Int 0, Var (v, _)), _)) -> [MOV (a, Var v)]
  | C.(Prim (Lor (Var (v, _), Int i), _))
   |C.(Prim (Lor (Int i, Var (v, _)), _)) -> [MOV (a, Var v); OR (a, Imm i)]
  | C.(Prim (Lor (Var (v1, _), Var (v2, _)), _)) when String.equal v1 v2 ->
      [MOV (a, Var v1)]
  | C.(Prim (Lor (Var (v1, _), Var (v2, _)), _)) ->
      [MOV (a, Var v1); OR (a, Var v2)]
  | C.(Prim (Lor _, _)) -> assert false
  (* lxor *)
  | C.(Prim (Lxor (Int i1, Int i2), _)) ->
      let i = i1 lxor i2 in
      if Int.(i = 0) then [XOR (a, a)] else [MOV (a, Imm i)]
  | C.(Prim (Lxor (Var (v, _), Int i), _))
   |C.(Prim (Lxor (Int i, Var (v, _)), _)) -> [MOV (a, Var v); XOR (a, Imm i)]
  | C.(Prim (Lxor (Var (v1, _), Var (v2, _)), _)) when String.equal v1 v2 ->
      [XOR (a, a)]
  | C.(Prim (Lxor (Var (v1, _), Var (v2, _)), _)) ->
      [MOV (a, Var v1); XOR (a, Var v2)]
  | C.(Prim (Lxor _, _)) -> assert false
  (* lnot *)
  | C.(Prim (Lnot (Int i), _)) -> [MOV (a, Imm (lnot i))]
  | C.(Prim (Lnot (Var (v, _)), _)) -> [MOV (a, Var v); NOT a]
  | C.(Prim (Lnot _, _)) -> assert false
  (* eq *)
  | C.(Prim (Eq (Int i1, Int i2), _)) ->
      if Int.(i1 = i2) then [MOV (a, Imm 1)] else [XOR (a, a)]
  | C.(Prim (Eq (Bool b1, Bool b2), _)) ->
      if Bool.equal b1 b2 then [MOV (a, Imm 1)] else [XOR (a, a)]
  | C.(Prim (Eq (Var (v, _), Int i), _))
   |C.(Prim (Eq (Int i, Var (v, _)), _)) ->
      [CMP (Var v, Imm i); SETCC (Cc.E, Bytereg AL); MOVZX (a, Bytereg AL)]
  | C.(Prim (Eq (Var (v, _), Bool b), _))
   |C.(Prim (Eq (Bool b, Var (v, _)), _)) ->
      let cc = if not b then Cc.E else Cc.NE in
      [TEST (Var v, Var v); SETCC (cc, Bytereg AL); MOVZX (a, Bytereg AL)]
  | C.(Prim (Eq (Var (v1, _), Var (v2, _)), _)) ->
      if String.equal v1 v2 then [MOV (a, Imm 1)]
      else
        [ CMP (Var v1, Var v2)
        ; SETCC (Cc.E, Bytereg AL)
        ; MOVZX (a, Bytereg AL) ]
  | C.(Prim (Eq _, _)) -> assert false
  (* lt *)
  | C.(Prim (Lt (Int i1, Int i2), _)) ->
      if Int.(i1 < i2) then [MOV (a, Imm 1)] else [XOR (a, a)]
  | C.(Prim (Lt (Var (v, _), Int i), _)) ->
      [CMP (Var v, Imm i); SETCC (Cc.L, Bytereg AL); MOVZX (a, Bytereg AL)]
  | C.(Prim (Lt (Int i, Var (v, _)), _)) ->
      [CMP (Var v, Imm i); SETCC (Cc.G, Bytereg AL); MOVZX (a, Bytereg AL)]
  | C.(Prim (Lt (Var (v1, _), Var (v2, _)), _)) ->
      if String.equal v1 v2 then [MOV (a, Imm 1)]
      else
        [ CMP (Var v1, Var v2)
        ; SETCC (Cc.L, Bytereg AL)
        ; MOVZX (a, Bytereg AL) ]
  | C.(Prim (Lt _, _)) -> assert false
  (* le *)
  | C.(Prim (Le (Int i1, Int i2), _)) ->
      if Int.(i1 <= i2) then [MOV (a, Imm 1)] else [XOR (a, a)]
  | C.(Prim (Le (Var (v, _), Int i), _)) ->
      [CMP (Var v, Imm i); SETCC (Cc.LE, Bytereg AL); MOVZX (a, Bytereg AL)]
  | C.(Prim (Le (Int i, Var (v, _)), _)) ->
      [CMP (Var v, Imm i); SETCC (Cc.GE, Bytereg AL); MOVZX (a, Bytereg AL)]
  | C.(Prim (Le (Var (v1, _), Var (v2, _)), _)) ->
      if String.equal v1 v2 then [MOV (a, Imm 1)]
      else
        [ CMP (Var v1, Var v2)
        ; SETCC (Cc.LE, Bytereg AL)
        ; MOVZX (a, Bytereg AL) ]
  | C.(Prim (Le _, _)) -> assert false
  (* gt *)
  | C.(Prim (Gt (Int i1, Int i2), _)) ->
      if Int.(i1 > i2) then [MOV (a, Imm 1)] else [XOR (a, a)]
  | C.(Prim (Gt (Var (v, _), Int i), _)) ->
      [CMP (Var v, Imm i); SETCC (Cc.G, Bytereg AL); MOVZX (a, Bytereg AL)]
  | C.(Prim (Gt (Int i, Var (v, _)), _)) ->
      [CMP (Var v, Imm i); SETCC (Cc.L, Bytereg AL); MOVZX (a, Bytereg AL)]
  | C.(Prim (Gt (Var (v1, _), Var (v2, _)), _)) ->
      if String.equal v1 v2 then [MOV (a, Imm 1)]
      else
        [ CMP (Var v1, Var v2)
        ; SETCC (Cc.G, Bytereg AL)
        ; MOVZX (a, Bytereg AL) ]
  | C.(Prim (Gt _, _)) -> assert false
  (* ge *)
  | C.(Prim (Ge (Int i1, Int i2), _)) ->
      if Int.(i1 >= i2) then [MOV (a, Imm 1)] else [XOR (a, a)]
  | C.(Prim (Ge (Var (v, _), Int i), _)) ->
      [CMP (Var v, Imm i); SETCC (Cc.GE, Bytereg AL); MOVZX (a, Bytereg AL)]
  | C.(Prim (Ge (Int i, Var (v, _)), _)) ->
      [CMP (Var v, Imm i); SETCC (Cc.LE, Bytereg AL); MOVZX (a, Bytereg AL)]
  | C.(Prim (Ge (Var (v1, _), Var (v2, _)), _)) ->
      if String.equal v1 v2 then [MOV (a, Imm 1)]
      else
        [ CMP (Var v1, Var v2)
        ; SETCC (Cc.GE, Bytereg AL)
        ; MOVZX (a, Bytereg AL) ]
  | C.(Prim (Ge _, _)) -> assert false
  (* not *)
  | C.(Prim (Not (Bool b), _)) -> if b then [XOR (a, a)] else [MOV (a, Imm 1)]
  | C.(Prim (Not (Var (v, _)), _)) -> [MOV (a, Var v); XOR (a, Imm 1)]
  | C.(Prim (Not _, _)) -> assert false
  (* vector-length *)
  | C.(Prim (Vectorlength (Var (_, Type.Vector ts)), _)) ->
      let len = List.length ts in
      if Int.(len = 0) then [XOR (a, a)] else [MOV (a, Imm len)]
  | C.(Prim (Vectorlength _, _)) -> assert false
  (* vector-ref *)
  | C.(Prim (Vectorref (Var (v, _), i), _)) ->
      [ MOV (Reg R11, Var v)
      ; MOV (a, Deref (Reg.R11, (i + total_tag_offset) * word_size)) ]
  | C.(Prim (Vectorref _, _)) -> assert false
  (* vector-set *)
  | C.(Prim (Vectorset (Var (v1, _), i, Int n), _)) ->
      [ MOV (Reg R11, Var v1)
      ; MOV (Deref (Reg.R11, (i + total_tag_offset) * word_size), Imm n)
      ; XOR (a, a) ]
  | C.(Prim (Vectorset (Var (v1, _), i, Bool b), _)) ->
      [ MOV (Reg R11, Var v1)
      ; MOV
          ( Deref (Reg.R11, (i + total_tag_offset) * word_size)
          , Imm (Bool.to_int b) )
      ; XOR (a, a) ]
  | C.(Prim (Vectorset (Var (v1, _), i, Void), _)) ->
      [ MOV (Reg R11, Var v1)
      ; MOV (Deref (Reg.R11, (i + total_tag_offset) * word_size), Imm 0)
      ; XOR (a, a) ]
  | C.(Prim (Vectorset (Var (v1, _), i, Var (v2, _)), _)) ->
      [ MOV (Reg R11, Var v1)
      ; MOV (Deref (Reg.R11, (i + total_tag_offset) * word_size), Var v2)
      ; XOR (a, a) ]
  | C.(Prim (Vectorset _, _)) -> assert false
  (* allocate *)
  | C.(Allocate (n, Type.Vector ts)) ->
      let vec_tag =
        let ptr_mask =
          List.foldi ts ~init:0 ~f:(fun i acc t ->
              match t with
              | C.Type.Vector _ -> (1 lsl i) lor acc
              | _ -> acc)
        in
        let len = List.length ts in
        (ptr_mask lsl 7) lor (len lsl 1) lor 1
      in
      let int_mask, bool_mask, void_mask =
        List.foldi ts ~init:(0, 0, 0)
          ~f:(fun i ((int_mask, bool_mask, void_mask) as acc) t ->
            match t with
            | C.Type.Integer -> ((1 lsl i) lor int_mask, bool_mask, void_mask)
            | C.Type.Boolean -> (int_mask, (1 lsl i) lor bool_mask, void_mask)
            | C.Type.Void -> (int_mask, bool_mask, (1 lsl i) lor void_mask)
            | _ -> acc)
      in
      [ MOV (Reg R11, Var Extern.free_ptr)
      ; ADD (Var Extern.free_ptr, Imm ((n + total_tag_offset) * word_size))
      ; MOV (Deref (Reg.R11, tag_offset * word_size), Imm vec_tag)
      ; MOV (Deref (Reg.R11, int_mask_offset * word_size), Imm int_mask)
      ; MOV (Deref (Reg.R11, bool_mask_offset * word_size), Imm bool_mask)
      ; MOV (Deref (Reg.R11, void_mask_offset * word_size), Imm void_mask)
      ; MOV (a, Reg R11) ]
  | C.(Allocate _) -> assert false
  (* global-value *)
  | C.Globalvalue (v, _) -> [MOV (a, Var v)]

let function_prologue rootstack_spills stack_space w =
  let setup_frame =
    if stack_space <= 0 then [] else [PUSH (Reg RBP); MOV (Reg RBP, Reg RSP)]
  in
  let callee_save_in_use =
    Set.fold w ~init:[] ~f:(fun acc a -> PUSH a :: acc) |> List.rev
  in
  let adj_sp =
    (* we need to align RSP to a 16-byte boundary
     * as prescribed by the System V ABI *)
    let adj = List.length callee_save_in_use mod 2 in
    if stack_space <= 0 then
      if adj <> 0 then [] else [SUB (Reg RSP, Imm word_size)]
    else [SUB (Reg RSP, Imm (stack_space + (word_size * adj)))]
  in
  let init =
    [ MOV (Reg RDI, Imm 0x4000)
      (* let's use a very small number to trigger the GC *)
    ; MOV (Reg RSI, Imm 16)
    ; CALL (Extern.initialize, 2)
    ; MOV (Reg R15, Var Extern.rootstack_begin)
    ; MOV (Deref (R15, 0), Imm 0)
    ; ADD (Reg R15, Imm (rootstack_spills * word_size)) ]
  in
  setup_frame @ callee_save_in_use @ adj_sp @ init

let function_epilogue rootstack_spills typ label stack_space w instrs =
  let callee_save_in_use =
    Set.fold w ~init:[] ~f:(fun acc a -> POP a :: acc)
  in
  let adj_rootstk = [SUB (Reg R15, Imm (rootstack_spills * word_size))] in
  let adj_sp =
    let adj = List.length callee_save_in_use mod 2 in
    if stack_space <= 0 then
      if adj <> 0 then [] else [ADD (Reg RSP, Imm word_size)]
    else [ADD (Reg RSP, Imm (stack_space + (word_size * adj)))]
  in
  let restore_frame = if stack_space <= 0 then [] else [POP (Reg RBP)] in
  let epilogue = adj_rootstk @ adj_sp @ callee_save_in_use @ restore_frame in
  let rec aux acc = function
    | [] ->
        failwith
          ("X.function_epilogue: block " ^ label ^ " is not well-formed")
    | RET :: _ ->
        let print =
          match typ with
          | C.Type.Integer ->
              [ PUSH (Reg RAX)
              ; MOV (Reg RDI, Reg RAX)
              ; CALL (Extern.print_int, 1)
              ; POP (Reg RAX) ]
          | C.Type.Boolean ->
              [ PUSH (Reg RAX)
              ; MOV (Reg RDI, Reg RAX)
              ; CALL (Extern.print_bool, 1)
              ; POP (Reg RAX) ]
          | C.Type.Void ->
              [PUSH (Reg RAX); CALL (Extern.print_void, 0); POP (Reg RAX)]
          | C.Type.Vector _ ->
              [ PUSH (Reg RAX)
              ; MOV (Reg RDI, Reg RAX)
              ; CALL (Extern.print_vector, 1)
              ; POP (Reg RAX) ]
        in
        List.rev acc @ print @ epilogue @ [RET]
    | instr :: rest -> aux (instr :: acc) rest
  in
  aux [] instrs

let rec patch_instructions = function
  | Program (info, blocks) ->
      let blocks =
        List.map blocks ~f:(fun (label, Block (_, info, instrs)) ->
            let instrs =
              List.map instrs ~f:patch_instructions_instr |> List.concat
            in
            (label, Block (label, info, instrs)))
      in
      (* 'w' is a set which exploits the ordering
       * for registers that Reg.t prescribes *)
      let w =
        List.fold blocks ~init:Args.empty
          ~f:(fun init (_, Block (_, _, instrs)) ->
            List.fold instrs ~init ~f:(fun w instr ->
                Set.union w (write_set instr)
                |> Set.filter ~f:(function
                     | Arg.Reg RSP -> false
                     | Arg.Reg RBP -> false
                     | Arg.Reg r -> Reg.is_callee_save r
                     | _ -> false)))
      in
      let blocks =
        List.map blocks ~f:(fun (label, block) ->
            (label, patch_instructions_block w info block))
      in
      Program (info, blocks)

and patch_instructions_block w info = function
  | Block (label, block_info, instrs) ->
      let instrs =
        if Label.equal label info.main then
          function_prologue info.rootstack_spills info.stack_space w @ instrs
        else instrs
      in
      let instrs =
        match List.last_exn instrs with
        | RET ->
            function_epilogue info.rootstack_spills info.typ label
              info.stack_space w instrs
        | _ -> instrs
      in
      Block (label, block_info, instrs)

and patch_instructions_instr = function
  | ADD ((Deref _ as d1), (Deref _ as d2)) ->
      [MOV (Reg RAX, d2); ADD (d1, Reg RAX)]
  | SUB ((Deref _ as d1), (Deref _ as d2)) ->
      [MOV (Reg RAX, d2); SUB (d1, Reg RAX)]
  | IMUL ((Deref _ as d), a) ->
      [MOV (Reg RAX, d); IMUL (Reg RAX, a); MOV (d, Reg RAX)]
  | IMULi ((Deref _ as d), a, i) ->
      [MOV (Reg RAX, d); IMULi (Reg RAX, a, i); MOV (d, Reg RAX)]
  | MOV ((Deref _ as d1), (Deref _ as d2)) ->
      [MOV (Reg RAX, d2); MOV (d1, Reg RAX)]
  | MOV (a1, a2) when Arg.equal a1 a2 -> []
  | XOR ((Deref _ as d1), (Deref _ as d2)) ->
      [MOV (Reg RAX, d2); XOR (d1, Reg RAX)]
  | AND ((Deref _ as d1), (Deref _ as d2)) ->
      [MOV (Reg RAX, d2); AND (d1, Reg RAX)]
  | OR ((Deref _ as d1), (Deref _ as d2)) ->
      [MOV (Reg RAX, d2); OR (d1, Reg RAX)]
  | CMP ((Deref _ as d1), (Deref _ as d2)) ->
      [MOV (Reg RAX, d2); CMP (d1, Reg RAX)]
  | TEST ((Deref _ as d1), (Deref _ as d2)) ->
      [MOV (Reg RAX, d2); TEST (d1, Reg RAX)]
  | MOVZX ((Deref _ as d), a) -> [MOVZX (Reg RAX, a); MOV (d, Reg RAX)]
  | instr -> [instr]

let rec uncover_live = function
  | Program (info, blocks) ->
      let la_map = Hashtbl.create (module Label) in
      let lb_map = Hashtbl.create (module Label) in
      let blocks' = Hashtbl.of_alist_exn (module Label) blocks in
      (* the CFG is currently a DAG, so we start from
       * the exit blocks and work our way backward
       * to the entry by visiting predecessors *)
      Cfg.iter_vertex
        (fun l ->
          if Cfg.out_degree info.cfg l = 0 then
            let block = Hashtbl.find_exn blocks' l in
            uncover_live_cfg blocks' info.cfg la_map lb_map block)
        info.cfg;
      let blocks =
        List.map blocks ~f:(fun (label, Block (_, info, instrs)) ->
            let live_after =
              match Hashtbl.find la_map label with
              | None -> List.map instrs ~f:(fun _ -> Args.empty)
              | Some la -> la
            in
            (label, Block (label, {live_after}, instrs)))
      in
      Program (info, blocks)

and uncover_live_cfg blocks cfg la_map lb_map = function
  | Block (label, _, instrs) ->
      let live_before =
        let lb =
          match Hashtbl.find lb_map label with
          | None ->
              if Cfg.out_degree cfg label = 0 then
                (* these registers are always live after an exit node *)
                Args.of_list [Reg RAX; Reg RSP; Reg RBP]
              else Args.empty
          | Some lb -> lb
        in
        Cfg.fold_succ
          (fun l acc ->
            let lb =
              match Hashtbl.find lb_map l with
              | None -> Args.empty
              | Some lb -> lb
            in
            Set.union lb acc)
          cfg label lb
      in
      let live_after, live_before =
        List.fold_right instrs
          ~init:([], [live_before])
          ~f:(fun instr (live_after, live_before) ->
            let live_after' = List.hd_exn live_before in
            let live_before' =
              Set.(
                union (diff live_after' (write_set instr)) (read_set instr))
            in
            (live_after' :: live_after, live_before' :: live_before))
      in
      Hashtbl.set la_map label live_after;
      Hashtbl.set lb_map label (List.hd_exn live_before);
      Cfg.iter_pred
        (fun l ->
          uncover_live_cfg blocks cfg la_map lb_map
            (Hashtbl.find_exn blocks l))
        cfg label

let rec build_interference = function
  | Program (info, blocks) ->
      let conflicts =
        let init = Interference_graph.empty in
        List.fold blocks ~init ~f:(fun g (_, block) ->
            build_interference_block info.locals_types g block)
      in
      Program ({info with conflicts}, blocks)

and build_interference_block locals_types g = function
  | Block (_, info, instrs) ->
      List.zip_exn info.live_after instrs
      |> List.fold ~init:g ~f:(fun g (la, instr) ->
             let w = write_set instr in
             Set.fold la ~init:g ~f:(fun g v ->
                 let default () =
                   Set.fold w ~init:g ~f:(fun g d ->
                       if Arg.equal v d then g
                       else Interference_graph.add_edge g v d)
                 in
                 match instr with
                 | MOV (d, s) | MOVZX (d, s) ->
                     if Arg.(equal v d || equal v s) then g
                     else Interference_graph.add_edge g v d
                 | XOR (d, s) when Arg.(equal d s) ->
                     (* special case, treat this like a MOV *)
                     if Arg.(equal v d) then g
                     else Interference_graph.add_edge g v d
                 | CALL (l, _) when String.equal l Extern.collect -> (
                   match v with
                   | Arg.Var v' when is_temp_var_name v' -> (
                     match Map.find_exn locals_types v' with
                     | C.Type.Vector _ ->
                         let g = default () in
                         Set.fold callee_save_set ~init:g ~f:(fun g d ->
                             Interference_graph.add_edge g v d)
                     | _ -> default () )
                   | _ -> default () )
                 | _ -> default ()))

let allocatable_regs =
  (* prioritize caller-save registers over callee-save registers *)
  [| Arg.Reg RCX
   ; Arg.Reg RDX
   ; Arg.Reg RSI
   ; Arg.Reg RDI
   ; Arg.Reg R8
   ; Arg.Reg R9
   ; Arg.Reg R10
   ; Arg.Reg RBX
   ; Arg.Reg R12
   ; Arg.Reg R13
   ; Arg.Reg R14 |]

let num_regs = Array.length allocatable_regs

let color_graph ?(bias = Interference_graph.empty) g =
  let q =
    Pairing_heap.create ~cmp:(fun (_, n) (_, m) -> Int.compare m n) ()
  in
  Interference_graph.iter_vertex
    (function
      | Arg.Var v as var when is_temp_var_name v ->
          Pairing_heap.add q (var, Interference_graph.in_degree g var)
      | _ -> ())
    g;
  let rec loop colors =
    match Pairing_heap.pop q with
    | None -> colors
    | Some (u, _) ->
        let assigned =
          Interference_graph.fold_succ
            (fun v assigned ->
              match Map.find colors v with
              | None -> assigned
              | Some c -> Set.add assigned c)
            g u Int.Set.empty
        in
        let bias_colors =
          try
            Interference_graph.succ bias u
            |> List.filter_map ~f:(fun v ->
                   Option.(
                     Map.find colors v
                     >>= fun c -> some_if (not (Set.mem assigned c)) c))
            |> Int.Set.of_list
          with Invalid_argument _ -> Int.Set.empty
        in
        let c =
          match Set.min_elt bias_colors with
          | Some c when c < num_regs -> c
          | _ ->
              let c = ref 0 in
              while Set.mem assigned !c do
                incr c
              done;
              !c
        in
        Map.set colors u c |> loop
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
      let bias =
        let init = Interference_graph.empty in
        List.fold blocks ~init ~f:(fun init (_, Block (_, _, instrs)) ->
            List.fold instrs ~init ~f:(fun bias instr ->
                match instr with
                | MOV ((Arg.Var v1 as d), (Arg.Var v2 as s))
                  when is_temp_var_name v1 && is_temp_var_name v2 ->
                    Interference_graph.add_edge bias d s
                | _ -> bias))
      in
      let colors = color_graph info.conflicts ~bias in
      let rootstack_spills = Hash_set.create (module String) in
      let blocks =
        List.map blocks ~f:(fun (label, block) ->
            ( label
            , allocate_registers_block rootstack_spills info.locals_types
                colors block ))
      in
      let stack_space =
        let colors =
          Map.filter_keys colors ~f:(function
            | Arg.Var v -> not (Hash_set.mem rootstack_spills v)
            | _ -> true)
        in
        match Map.data colors |> Int.Set.of_list |> Set.max_elt with
        | None -> 0
        | Some c -> max (c - num_regs + 1) 0 * word_size
      in
      Program
        ( { info with
            stack_space
          ; rootstack_spills= Hash_set.length rootstack_spills }
        , blocks )

and allocate_registers_block rootstack_spills locals_types colors = function
  | Block (label, info, instrs) ->
      let instrs =
        List.map instrs
          ~f:(allocate_registers_instr rootstack_spills locals_types colors)
      in
      Block (label, info, instrs)

and allocate_registers_instr rootstack_spills locals_types colors instr =
  let color = color_arg rootstack_spills locals_types colors in
  match instr with
  | ADD (a1, a2) ->
      let a1 = color a1 in
      let a2 = color a2 in
      ADD (a1, a2)
  | SUB (a1, a2) ->
      let a1 = color a1 in
      let a2 = color a2 in
      SUB (a1, a2)
  | IMUL (a1, a2) ->
      let a1 = color a1 in
      let a2 = color a2 in
      IMUL (a1, a2)
  | IMULi (a1, a2, a3) ->
      let a1 = color a1 in
      let a2 = color a2 in
      IMULi (a1, a2, a3)
  | IDIV a ->
      let a = color a in
      IDIV a
  | NEG a ->
      let a = color a in
      NEG a
  | MOV (a1, a2) ->
      let a1 = color a1 in
      let a2 = color a2 in
      MOV (a1, a2)
  | CALL _ as c -> c
  | PUSH a ->
      let a = color a in
      PUSH a
  | POP a ->
      let a = color a in
      POP a
  | RET -> RET
  | JMP _ as j -> j
  | NOT a ->
      let a = color a in
      NOT a
  | XOR (a1, a2) ->
      let a1 = color a1 in
      let a2 = color a2 in
      XOR (a1, a2)
  | AND (a1, a2) ->
      let a1 = color a1 in
      let a2 = color a2 in
      AND (a1, a2)
  | OR (a1, a2) ->
      let a1 = color a1 in
      let a2 = color a2 in
      OR (a1, a2)
  | CMP (a1, a2) ->
      let a1 = color a1 in
      let a2 = color a2 in
      CMP (a1, a2)
  | TEST (a1, a2) ->
      let a1 = color a1 in
      let a2 = color a2 in
      TEST (a1, a2)
  | SETCC _ as s -> s
  | MOVZX (a1, a2) ->
      let a1 = color a1 in
      MOVZX (a1, a2)
  | JCC _ as j -> j

and color_arg rootstack_spills locals_types colors = function
  | Arg.Var v as a when is_temp_var_name v -> (
    match Map.find colors a with
    | None -> failwith ("X.color_arg: var " ^ v ^ " was not colored")
    | Some c -> (
        assert (c >= 0);
        if c < num_regs then allocatable_regs.(c)
        else
          let offset = (c - num_regs + 1) * word_size in
          match Map.find_exn locals_types v with
          | C.Type.Vector _ ->
              Hash_set.add rootstack_spills v;
              Deref (R15, -(offset - word_size))
          | _ -> Deref (RBP, -offset) ) )
  | a -> a

let rec remove_jumps = function
  | Program (info, blocks) ->
      let cfg, blocks = remove_jumps_aux info.cfg blocks in
      Program ({info with cfg}, blocks)

and remove_jumps_aux cfg blocks =
  let afters = Hashtbl.create (module Label) in
  let rec interleave_pairs = function
    | [] -> []
    | (x, _) :: (y, b) :: rest -> (x, y) :: interleave_pairs ((y, b) :: rest)
    | [x] -> []
  in
  List.iter (interleave_pairs blocks) ~f:(fun (x, y) ->
      Hashtbl.set afters x y);
  let blocks' = Hashtbl.of_alist_exn (module Label) blocks in
  let merged = Hashtbl.create (module Label) in
  let merge_info info info' =
    {live_after= List.drop_last_exn info.live_after @ info'.live_after}
  in
  let blocks =
    List.filter_map blocks ~f:(fun ((label, Block (_, info, instrs)) as b) ->
        if Hashtbl.mem merged label then None
        else
          match List.last_exn instrs with
          | JMP label' when not (Label.equal label label') -> (
              if Cfg.in_degree cfg label' = 1 then (
                let (Block (_, info', instrs')) =
                  Hashtbl.find_exn blocks' label'
                in
                let instrs = List.drop_last_exn instrs @ instrs' in
                Hashtbl.set merged label' label;
                Some (label, Block (label, merge_info info info', instrs)) )
              else
                match Hashtbl.find afters label with
                | Some label'' when Label.equal label' label'' -> (
                  match List.drop_last instrs with
                  | None -> Some b
                  | Some instrs ->
                      let info =
                        {live_after= List.drop_last_exn info.live_after}
                      in
                      Some (label, Block (label, info, instrs)) )
                | _ -> Some b )
          | _ -> Some b)
  in
  let cfg =
    Hashtbl.fold merged ~init:cfg ~f:(fun ~key:l ~data:l' cfg ->
        let cfg =
          Cfg.fold_succ (fun l cfg -> Cfg.add_edge cfg l' l) cfg l cfg
        in
        Cfg.remove_vertex cfg l)
  in
  if Hashtbl.is_empty merged then (cfg, blocks)
  else remove_jumps_aux cfg blocks

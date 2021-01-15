open Core_kernel

type label = string

type 'a label_map = 'a C.label_map

let empty_label_map = C.empty_label_map

let word_size = 8

module Cc = struct
  type t = E | L | LE | G | GE

  let to_string = function
    | E -> "e"
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
      | Var v -> v
  end

  include T
  include Comparable.Make (T)
end

module Args = Set.Make (Arg)
module Arg_map = Map.Make (Arg)
module Interference_graph = Graph.Persistent.Graph.Concrete (Arg)
module Cfg = C.Cfg

type info =
  { main: label
  ; stack_space: int
  ; conflicts: Interference_graph.t
  ; typ: C.Type.t
  ; cfg: Cfg.t }

type t = Program of info * blocks

and blocks = (label * block) list

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
  | NOT of arg
  | XOR of arg * arg
  | CMP of arg * arg
  | SETCC of Cc.t * arg
  | MOVZX of arg * arg
  | JCC of Cc.t * label

and arg = Arg.t

let read_int = "read_int"

let print_int = "print_int"

let print_bool = "print_bool"

let externs = [read_int; print_int; print_bool]

let rec to_string = function
  | Program (info, blocks) ->
      let blks =
        List.map blocks ~f:snd
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
  | IMUL (a1, a2) ->
      Printf.sprintf "imul %s, %s" (Arg.to_string a1) (Arg.to_string a2)
  | IMULi (a1, a2, a3) ->
      Printf.sprintf "imul %s, %s, %s" (Arg.to_string a1) (Arg.to_string a2)
        (Arg.to_string a3)
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
  | CMP (a1, a2) ->
      Printf.sprintf "cmp %s, %s" (Arg.to_string a1) (Arg.to_string a2)
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
        List.fold tails ~init:[] ~f:(fun blocks (label, tail) ->
            let instrs = select_instructions_tail tails tail in
            (label, Block (label, block_info, instrs)) :: blocks)
      in
      let info =
        { main= info.main
        ; stack_space= 0
        ; conflicts= Interference_graph.empty
        ; typ= info.typ
        ; cfg= info.cfg }
      in
      Program (info, blocks |> List.rev)

and select_instructions_tail tails t =
  let open Arg in
  match t with
  | C.Return e -> select_instruction_exp (Reg RAX) e @ [RET]
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
  | C.If ((cmp, Var v, Int i), lt, lf) ->
      let cc = Cc.of_c_cmp cmp in
      [CMP (Var v, Imm i); JCC (cc, lt); JMP lf]
  | C.If ((cmp, Int i, Var v), lt, lf) ->
      let cc = Cc.of_c_cmp_swap cmp in
      [CMP (Var v, Imm i); JCC (cc, lt); JMP lf]
  | C.If ((cmp, Var v, Bool b), lt, lf) -> (
    match cmp with
    | Eq -> [CMP (Var v, Imm (Bool.to_int b)); JCC (Cc.E, lt); JMP lf]
    | _ -> assert false )
  | C.If ((cmp, Bool b, Var v), lt, lf) -> (
    match cmp with
    | Eq -> [CMP (Var v, Imm (Bool.to_int b)); JCC (Cc.E, lt); JMP lf]
    | _ -> assert false )
  | C.If ((cmp, Var v1, Var v2), lt, lf) when String.equal v1 v2 -> (
    match cmp with
    | Eq -> [JMP lt]
    | _ -> [JMP lf] )
  | C.If ((cmp, Var v1, Var v2), lt, lf) ->
      let cc = Cc.of_c_cmp cmp in
      [CMP (Var v1, Var v2); JCC (cc, lt); JMP lf]
  | C.If _ -> assert false

and select_instructions_stmt s =
  let open Arg in
  match s with
  | C.Assign (v, e) -> select_instruction_exp (Var v) e

and select_instruction_exp a p =
  let open Arg in
  match p with
  (* atom *)
  | C.(Atom (Int i)) -> if Int.(i = 0) then [XOR (a, a)] else [MOV (a, Imm i)]
  | C.(Atom (Bool b)) -> if not b then [XOR (a, a)] else [MOV (a, Imm 1)]
  | C.(Atom (Var v)) -> [MOV (a, Var v)]
  (* read *)
  | C.(Prim Read) -> (
      let c = CALL (read_int, 0) in
      match a with
      | Arg.Reg RAX -> [c]
      | _ -> [c; MOV (a, Reg RAX)] )
  (* minus *)
  | C.(Prim (Minus (Int i))) -> [MOV (a, Imm (-i))]
  | C.(Prim (Minus (Var v))) -> [MOV (a, Var v); NEG a]
  | C.(Prim (Minus _)) -> assert false
  (* plus *)
  | C.(Prim (Plus (Int i1, Int i2))) ->
      let i = i1 + i2 in
      if Int.(i = 0) then [XOR (a, a)] else [MOV (a, Imm i)]
  | C.(Prim (Plus (Var v, Int i))) | C.(Prim (Plus (Int i, Var v))) ->
      [MOV (a, Var v); ADD (a, Imm i)]
  | C.(Prim (Plus (Var v1, Var v2))) -> [MOV (a, Var v1); ADD (a, Var v2)]
  | C.(Prim (Plus _)) -> assert false
  (* subtract *)
  | C.(Prim (Subtract (Int i1, Int i2))) ->
      let i = i1 - i2 in
      if Int.(i = 0) then [XOR (a, a)] else [MOV (a, Imm (i1 - i2))]
  | C.(Prim (Subtract (Var v, Int i))) -> [MOV (a, Var v); SUB (a, Imm i)]
  | C.(Prim (Subtract (Int i, Var v))) -> [MOV (a, Imm i); SUB (a, Var v)]
  | C.(Prim (Subtract (Var v1, Var v2))) -> [MOV (a, Var v1); SUB (a, Var v2)]
  | C.(Prim (Subtract _)) -> assert false
  (* mult *)
  | C.(Prim (Mult (_, Int 0))) | C.(Prim (Mult (Int 0, _))) -> [XOR (a, a)]
  | C.(Prim (Mult (Int i1, Int i2))) ->
      let i = i1 * i2 in
      if Int.(i = 0) then [XOR (a, a)] else [MOV (a, Imm (i1 * i2))]
  | C.(Prim (Mult (Var v, Int i))) | C.(Prim (Mult (Int i, Var v))) ->
      if fits_int32 i then [IMULi (a, Var v, Imm i)]
      else [MOV (a, Imm i); IMUL (a, Var v)]
  | C.(Prim (Mult (Var v1, Var v2))) -> [MOV (a, Var v1); IMUL (a, Var v2)]
  | C.(Prim (Mult _)) -> assert false
  (* eq *)
  | C.(Prim (Eq (Int i1, Int i2))) ->
      if Int.(i1 = i2) then [MOV (a, Imm 1)] else [XOR (a, a)]
  | C.(Prim (Eq (Bool b1, Bool b2))) ->
      if Bool.equal b1 b2 then [MOV (a, Imm 1)] else [XOR (a, a)]
  | C.(Prim (Eq (Var v, Int i))) | C.(Prim (Eq (Int i, Var v))) ->
      [CMP (Var v, Imm i); SETCC (Cc.E, Bytereg AL); MOVZX (a, Bytereg AL)]
  | C.(Prim (Eq (Var v1, Var v2))) ->
      if String.equal v1 v2 then [MOV (a, Imm 1)]
      else
        [ CMP (Var v1, Var v2)
        ; SETCC (Cc.E, Bytereg AL)
        ; MOVZX (a, Bytereg AL) ]
  | C.(Prim (Eq _)) -> assert false
  (* lt *)
  | C.(Prim (Lt (Int i1, Int i2))) ->
      if Int.(i1 < i2) then [MOV (a, Imm 1)] else [XOR (a, a)]
  | C.(Prim (Lt (Var v, Int i))) ->
      [CMP (Var v, Imm i); SETCC (Cc.L, Bytereg AL); MOVZX (a, Bytereg AL)]
  | C.(Prim (Lt (Int i, Var v))) ->
      [CMP (Var v, Imm i); SETCC (Cc.G, Bytereg AL); MOVZX (a, Bytereg AL)]
  | C.(Prim (Lt (Var v1, Var v2))) ->
      if String.equal v1 v2 then [MOV (a, Imm 1)]
      else
        [ CMP (Var v1, Var v2)
        ; SETCC (Cc.L, Bytereg AL)
        ; MOVZX (a, Bytereg AL) ]
  | C.(Prim (Lt _)) -> assert false
  (* le *)
  | C.(Prim (Le (Int i1, Int i2))) ->
      if Int.(i1 <= i2) then [MOV (a, Imm 1)] else [XOR (a, a)]
  | C.(Prim (Le (Var v, Int i))) ->
      [CMP (Var v, Imm i); SETCC (Cc.LE, Bytereg AL); MOVZX (a, Bytereg AL)]
  | C.(Prim (Le (Int i, Var v))) ->
      [CMP (Var v, Imm i); SETCC (Cc.GE, Bytereg AL); MOVZX (a, Bytereg AL)]
  | C.(Prim (Le (Var v1, Var v2))) ->
      if String.equal v1 v2 then [MOV (a, Imm 1)]
      else
        [ CMP (Var v1, Var v2)
        ; SETCC (Cc.LE, Bytereg AL)
        ; MOVZX (a, Bytereg AL) ]
  | C.(Prim (Le _)) -> assert false
  (* gt *)
  | C.(Prim (Gt (Int i1, Int i2))) ->
      if Int.(i1 > i2) then [MOV (a, Imm 1)] else [XOR (a, a)]
  | C.(Prim (Gt (Var v, Int i))) ->
      [CMP (Var v, Imm i); SETCC (Cc.G, Bytereg AL); MOVZX (a, Bytereg AL)]
  | C.(Prim (Gt (Int i, Var v))) ->
      [CMP (Var v, Imm i); SETCC (Cc.L, Bytereg AL); MOVZX (a, Bytereg AL)]
  | C.(Prim (Gt (Var v1, Var v2))) ->
      if String.equal v1 v2 then [MOV (a, Imm 1)]
      else
        [ CMP (Var v1, Var v2)
        ; SETCC (Cc.G, Bytereg AL)
        ; MOVZX (a, Bytereg AL) ]
  | C.(Prim (Gt _)) -> assert false
  (* ge *)
  | C.(Prim (Ge (Int i1, Int i2))) ->
      if Int.(i1 >= i2) then [MOV (a, Imm 1)] else [XOR (a, a)]
  | C.(Prim (Ge (Var v, Int i))) ->
      [CMP (Var v, Imm i); SETCC (Cc.GE, Bytereg AL); MOVZX (a, Bytereg AL)]
  | C.(Prim (Ge (Int i, Var v))) ->
      [CMP (Var v, Imm i); SETCC (Cc.LE, Bytereg AL); MOVZX (a, Bytereg AL)]
  | C.(Prim (Ge (Var v1, Var v2))) ->
      if String.equal v1 v2 then [MOV (a, Imm 1)]
      else
        [ CMP (Var v1, Var v2)
        ; SETCC (Cc.GE, Bytereg AL)
        ; MOVZX (a, Bytereg AL) ]
  | C.(Prim (Ge _)) -> assert false
  (* not *)
  | C.(Prim (Not (Bool b))) -> if b then [XOR (a, a)] else [MOV (a, Imm 1)]
  | C.(Prim (Not (Var v))) -> [MOV (a, Var v); NOT a]
  | C.(Prim (Not _)) -> assert false

let is_temp_var_name = String.is_prefix ~prefix:"%"

let filter_non_locations =
  Set.filter ~f:(function
    | Arg.Imm _ -> false
    | _ -> true)

let caller_save_set =
  List.map Reg.caller_save ~f:(fun r -> Arg.Reg r) |> Args.of_list

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
     |SETCC (_, a)
     |MOVZX (a, _) -> Args.singleton a
    | CALL _ -> Set.add caller_save_set (Reg RSP)
    | PUSH _ | RET -> Args.singleton (Reg RSP)
    | POP a -> Args.of_list [a; Reg RSP]
    | JMP _ | JCC _ | CMP _ -> Args.empty
  in
  aux instr |> filter_non_locations |> Args.map ~f:convert_bytereg

let read_set instr =
  let aux = function
    (* special case, it DOES read the source register,
     * but in effect it's just zeroing the destination. *)
    | XOR (a1, a2) when Arg.equal a1 a2 -> Args.empty
    | ADD (a1, a2)
     |SUB (a1, a2)
     |IMUL (a1, a2)
     |XOR (a1, a2)
     |CMP (a1, a2) -> Args.of_list [a1; a2]
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
  aux instr |> filter_non_locations

let function_prologue stack_space w =
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
  setup_frame @ callee_save_in_use @ adj_sp

let function_epilogue typ label stack_space w instrs =
  let callee_save_in_use =
    Set.fold w ~init:[] ~f:(fun acc a -> POP a :: acc)
  in
  let adj_sp =
    let adj = List.length callee_save_in_use mod 2 in
    if stack_space <= 0 then
      if adj <> 0 then [] else [ADD (Reg RSP, Imm word_size)]
    else [ADD (Reg RSP, Imm (stack_space + (word_size * adj)))]
  in
  let restore_frame = if stack_space <= 0 then [] else [POP (Reg RBP)] in
  let epilogue = adj_sp @ callee_save_in_use @ restore_frame in
  let rec aux acc = function
    | [] ->
        failwith
          ( "X.insert_function_epilogue: block " ^ label
          ^ " is not well-formed" )
    | RET :: _ ->
        let print =
          match typ with
          | C.Type.Integer -> print_int
          | C.Type.Boolean -> print_bool
        in
        List.rev acc
        @ [MOV (Reg RDI, Reg RAX); CALL (print, 1)]
        @ epilogue @ [RET]
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
        if String.equal label info.main then
          function_prologue info.stack_space w @ instrs
        else instrs
      in
      let instrs =
        if Cfg.out_degree info.cfg label = 0 then
          function_epilogue info.typ label info.stack_space w instrs
        else instrs
      in
      Block (label, block_info, instrs)

and patch_instructions_instr = function
  | ADD ((Deref _ as d1), (Deref _ as d2)) ->
      [MOV (Reg RAX, d2); ADD (d1, Reg RAX)]
  | SUB ((Deref _ as d1), (Deref _ as d2)) ->
      [MOV (Reg RAX, d2); SUB (d1, Reg RAX)]
  | MOV ((Deref _ as d1), (Deref _ as d2)) ->
      [MOV (Reg RAX, d2); MOV (d1, Reg RAX)]
  | MOV (a1, a2) when Arg.equal a1 a2 -> []
  | IMUL ((Deref _ as d), a) ->
      [MOV (Reg RAX, d); IMUL (Reg RAX, a); MOV (d, Reg RAX)]
  | IMULi ((Deref _ as d), a, i) ->
      [MOV (Reg RAX, d); IMULi (Reg RAX, a, i); MOV (d, Reg RAX)]
  | instr -> [instr]

let rec uncover_live = function
  | Program (info, blocks) ->
      let la_map = Hashtbl.create (module String) in
      let lb_map = Hashtbl.create (module String) in
      let blocks' = Hashtbl.of_alist_exn (module String) blocks in
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
          | None -> Args.empty
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
            let live_before' = List.hd_exn live_before in
            let live_after' = live_before' in
            let w = write_set instr in
            let r = read_set instr in
            let live_before'' = Set.(union (diff live_after' w) r) in
            (live_after' :: live_after, live_before'' :: live_before))
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
            build_interference_block g block)
      in
      Program ({info with conflicts}, blocks)

and build_interference_block g = function
  | Block (_, info, instrs) ->
      List.zip_exn info.live_after instrs
      |> List.fold ~init:g ~f:(fun g (la, instr) ->
             let w = write_set instr in
             Set.fold la ~init:g ~f:(fun g v ->
                 match instr with
                 | MOV (d, s) | MOVZX (d, s) ->
                     if Arg.(equal v d || equal v s) then g
                     else Interference_graph.add_edge g v d
                 | XOR (d, s) when Arg.(equal d s) ->
                     (* special case, treat this like a MOV *)
                     if Arg.(equal v d || equal v s) then g
                     else Interference_graph.add_edge g v d
                 | _ ->
                     Set.fold w ~init:g ~f:(fun g d ->
                         if Arg.equal v d then g
                         else Interference_graph.add_edge g v d)))

let allocatable_regs =
  (* prioritize caller-save registers over callee-save registers *)
  [| Arg.Reg RCX
   ; Arg.Reg RDX
   ; Arg.Reg RSI
   ; Arg.Reg RDI
   ; Arg.Reg R8
   ; Arg.Reg R9
   ; Arg.Reg R10
   ; Arg.Reg R11
   ; Arg.Reg RBX
   ; Arg.Reg R12
   ; Arg.Reg R13
   ; Arg.Reg R14
   ; Arg.Reg R15 |]

let num_regs = Array.length allocatable_regs

let color_graph g =
  let cmp (_, n) (_, m) = Int.compare m n in
  let q = Pairing_heap.create ~cmp () in
  Interference_graph.iter_vertex
    (fun v -> Pairing_heap.add q (v, Interference_graph.in_degree g v))
    g;
  let rec loop colors =
    match Pairing_heap.pop q with
    | None -> colors
    | Some (u, _) -> (
      match u with
      | Arg.Var _ ->
          let assigned =
            Interference_graph.fold_succ
              (fun v assigned ->
                match Map.find colors v with
                | None -> assigned
                | Some c -> Set.add assigned c)
              g u Int.Set.empty
          in
          let c = ref 0 in
          while Set.mem assigned !c do
            incr c
          done;
          loop (Map.set colors u !c)
      | _ -> loop colors )
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
      let colors = color_graph info.conflicts in
      let blocks =
        List.map blocks ~f:(fun (label, block) ->
            (label, allocate_registers_block colors block))
      in
      let stack_space =
        match Map.data colors |> Int.Set.of_list |> Set.max_elt with
        | None -> 0
        | Some c -> max (c - num_regs + 1) 0 * word_size
      in
      Program ({info with stack_space}, blocks)

and allocate_registers_block colors = function
  | Block (label, info, instrs) ->
      let instrs = List.map instrs ~f:(allocate_registers_instr colors) in
      Block (label, info, instrs)

and allocate_registers_instr colors = function
  | ADD (a1, a2) ->
      let a1 = color_arg colors a1 in
      let a2 = color_arg colors a2 in
      ADD (a1, a2)
  | SUB (a1, a2) ->
      let a1 = color_arg colors a1 in
      let a2 = color_arg colors a2 in
      SUB (a1, a2)
  | IMUL (a1, a2) ->
      let a1 = color_arg colors a1 in
      let a2 = color_arg colors a2 in
      IMUL (a1, a2)
  | IMULi (a1, a2, a3) ->
      let a1 = color_arg colors a1 in
      let a2 = color_arg colors a2 in
      IMULi (a1, a2, a3)
  | NEG a ->
      let a = color_arg colors a in
      NEG a
  | MOV (a1, a2) ->
      let a1 = color_arg colors a1 in
      let a2 = color_arg colors a2 in
      MOV (a1, a2)
  | CALL _ as c -> c
  | PUSH a ->
      let a = color_arg colors a in
      PUSH a
  | POP a ->
      let a = color_arg colors a in
      POP a
  | RET -> RET
  | JMP _ as j -> j
  | NOT a ->
      let a = color_arg colors a in
      NOT a
  | XOR (a1, a2) ->
      let a1 = color_arg colors a1 in
      let a2 = color_arg colors a2 in
      XOR (a1, a2)
  | CMP (a1, a2) ->
      let a1 = color_arg colors a1 in
      let a2 = color_arg colors a2 in
      CMP (a1, a2)
  | SETCC _ as s -> s
  | MOVZX (a1, a2) ->
      let a1 = color_arg colors a1 in
      MOVZX (a1, a2)
  | JCC _ as j -> j

and color_arg colors = function
  | Arg.Var v as a when is_temp_var_name v -> (
    match Map.find colors a with
    | None -> failwith ("X.color_arg: var " ^ v ^ " was not colored")
    | Some c ->
        assert (c >= 0);
        if c < num_regs then allocatable_regs.(c)
        else
          let offset = (c - num_regs + 1) * word_size in
          Deref (RBP, -offset) )
  | a -> a

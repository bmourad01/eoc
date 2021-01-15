open Core_kernel

let () =
  let prog = Eoc.Parse_r.parse Sys.argv.(1) in
  let prog =
    Eoc.(C.explicate_control (R_anf.resolve_complex R.(uniquify prog)))
  in
  match Eoc.C.interp prog with
  | `Int i -> Printf.printf "%d\n" i
  | `Bool b -> Printf.printf "%s\n" (if b then "#t" else "#f")

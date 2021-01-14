open Core_kernel
open Eoc

let () =
  let prog = Parse_r.parse Sys.argv.(1) in
  print_endline
    X.(
      to_string
        (select_instructions
           (C.explicate_control (R_anf.resolve_complex (R.uniquify prog)))))

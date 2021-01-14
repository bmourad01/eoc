open Core_kernel

let () =
  let prog = Eoc.Parse_r.parse Sys.argv.(1) in
  print_endline
    Eoc.(
      C.(
        to_string
          (explicate_control (R_anf.resolve_complex R.(uniquify prog)))))

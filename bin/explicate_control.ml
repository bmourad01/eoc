open Core_kernel

let () =
  let prog = Eoc.Parse_r.parse Sys.argv.(1) in
  print_endline
    Eoc.C.(
      to_string (explicate_control (Eoc.R_anf.rco Eoc.R.(uniquify prog))))

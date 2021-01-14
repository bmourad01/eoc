open Core_kernel

let () =
  let prog = Eoc.Parse_r.parse Sys.argv.(1) in
  print_endline
    Eoc.X.(
      to_string
        (select_instructions
           (Eoc.C.explicate_control (Eoc.R_anf.rco Eoc.R.(uniquify prog)))))

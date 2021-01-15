open Core_kernel

let () =
  let prog = Eoc.Parse_r.parse Sys.argv.(1) in
  print_endline
    Eoc.(
      X.(
        to_string
          (patch_instructions
             (allocate_registers
                (build_interference
                   (uncover_live
                      (select_instructions
                         (C.explicate_control
                            (R_anf.resolve_complex R.(uniquify prog))))))))))
open Core_kernel

let () =
  let prog = Eoc.Parse_r.parse Sys.argv.(1) in
  let Eoc.X.(Program (info, blocks)) =
    Eoc.(
      X.(
        build_interference
          (uncover_live
             (select_instructions
                (C.explicate_control
                   (R_anf.resolve_complex R.(uniquify prog)))))))
  in
  let colors = Eoc.X.(color_graph info.conflicts) in
  Map.iteri colors ~f:(fun ~key ~data ->
      Printf.printf "%s -> %d\n" (Eoc.X.Arg.to_string key) data)

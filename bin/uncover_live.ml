open Core_kernel

let () =
  let prog = Eoc.Parse_r.parse Sys.argv.(1) in
  let Eoc.X.(Program (_, blocks)) =
    Eoc.(
      X.(
        uncover_live
          (select_instructions
             (C.explicate_control
                (R_anf.resolve_complex R.(uniquify (opt prog)))))))
  in
  Map.iter blocks ~f:(fun Eoc.X.(Block (label, info, instrs)) ->
      let l = List.zip_exn Eoc.X.(info.live_after) instrs in
      Printf.printf "%s:\n" label;
      List.iter l ~f:(fun (la, instr) ->
          Printf.printf "    %s ; %s\n"
            (Eoc.X.string_of_instr instr)
            ( Set.to_list la
            |> List.map ~f:Eoc.X.Arg.to_string
            |> String.concat ~sep:", " |> Printf.sprintf "{%s}" )))

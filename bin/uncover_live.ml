open Core_kernel

let () =
  let Eoc.X.(Program (_, defs)) =
    Eoc.Parse_r.parse Sys.argv.(1)
    |> Eoc.R_typed.uniquify |> Eoc.R_alloc.expose_allocation
    |> Eoc.R_anf.resolve_complex |> Eoc.C.explicate_control
    |> Eoc.C.optimize_jumps |> Eoc.X.select_instructions
    |> Eoc.X.uncover_live
  in
  List.iter defs ~f:(fun (Def (_, _, blocks)) ->
      List.iter blocks ~f:(fun (_, Eoc.X.(Block (label, info, instrs))) ->
          let l = List.zip_exn Eoc.X.(info.live_after) instrs in
          Printf.printf "%s:\n" label;
          List.iter l ~f:(fun (la, instr) ->
              Printf.printf "    %s ; %s\n"
                (Eoc.X.string_of_instr instr)
                ( Set.to_list la
                |> List.map ~f:Eoc.X.Arg.to_string
                |> String.concat ~sep:", " |> Printf.sprintf "{%s}" )));
      Printf.printf "\n")

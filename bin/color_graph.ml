open Core_kernel

let () =
  let Eoc.X.(Program (info, defs)) =
    Eoc.Parse_r.parse Sys.argv.(1)
    |> Eoc.R_typed.uniquify |> Eoc.R_typed.convert_to_closures
    |> Eoc.R_typed.limit_functions |> Eoc.R_alloc.expose_allocation
    |> Eoc.R_anf.resolve_complex |> Eoc.C.explicate_control
    |> Eoc.C.optimize_jumps |> Eoc.X.select_instructions
    |> Eoc.X.uncover_live |> Eoc.X.build_interference
  in
  List.iter defs ~f:(fun (Def (info, l, _)) ->
      let colors = Eoc.X.(color_graph info.conflicts) in
      Printf.printf "%s:\n\n" l;
      Map.iteri colors ~f:(fun ~key ~data ->
          Printf.printf "%s -> %d\n" (Eoc.X.Arg.to_string key) data);
      Printf.printf "\n")

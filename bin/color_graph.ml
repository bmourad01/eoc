open Core_kernel

let () =
  let Eoc.X.(Program (info, _)) =
    Eoc.Parse_r.parse Sys.argv.(1)
    |> Eoc.R_typed.uniquify |> Eoc.R_alloc.expose_allocation
    |> Eoc.R_anf.resolve_complex |> Eoc.C.explicate_control
    |> Eoc.C.optimize_jumps |> Eoc.X.select_instructions
    |> Eoc.X.uncover_live |> Eoc.X.build_interference
  in
  let colors = Eoc.X.(color_graph info.conflicts) in
  Map.iteri colors ~f:(fun ~key ~data ->
      Printf.printf "%s -> %d\n" (Eoc.X.Arg.to_string key) data)

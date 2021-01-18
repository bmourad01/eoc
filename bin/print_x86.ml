open Core_kernel

let () =
  Eoc.Parse_r.parse Sys.argv.(1)
  |> Eoc.R_typed.uniquify |> Eoc.R_alloc.expose_allocation
  |> Eoc.R_anf.resolve_complex |> Eoc.C.explicate_control
  |> Eoc.C.optimize_jumps |> Eoc.X.select_instructions |> Eoc.X.uncover_live
  |> Eoc.X.build_interference |> Eoc.X.allocate_registers
  |> Eoc.X.remove_jumps |> Eoc.X.patch_instructions |> Eoc.X.to_string
  |> print_endline

open Core_kernel

let () =
  Eoc.Parse_r.parse Sys.argv.(1)
  |> Eoc.R.uniquify |> Eoc.R_anf.resolve_complex |> Eoc.C.explicate_control
  |> Eoc.X.select_instructions |> Eoc.X.uncover_live
  |> Eoc.X.build_interference |> Eoc.X.allocate_registers
  |> Eoc.X.remove_jumps |> Eoc.X.patch_instructions |> Eoc.X.to_string
  |> print_endline

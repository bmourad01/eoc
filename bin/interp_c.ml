open Core_kernel

let () =
  Eoc.Parse_r.parse Sys.argv.(1)
  |> Eoc.R.uniquify |> Eoc.R_alloc.expose_allocation
  |> Eoc.R_anf.resolve_complex |> Eoc.C.explicate_control |> Eoc.C.interp
  |> Eoc.R.string_of_answer |> print_endline

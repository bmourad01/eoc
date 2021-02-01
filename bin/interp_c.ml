open Core_kernel

let () =
  Eoc.Parse_r.parse Sys.argv.(1)
  |> Eoc.R_typed.uniquify |> Eoc.R_typed.convert_assignments
  |> Eoc.R_typed.convert_to_closures |> Eoc.R_typed.limit_functions
  |> Eoc.R_alloc.expose_allocation |> Eoc.R_anf.resolve_complex
  |> Eoc.C.explicate_control |> Eoc.C.interp |> Eoc.C.string_of_answer
  |> print_endline

open Core_kernel

let () =
  Eoc.Parse_r.parse Sys.argv.(1)
  |> Eoc.R_typed.uniquify |> Eoc.R_alloc.expose_allocation
  |> Eoc.R_alloc.to_string |> print_endline

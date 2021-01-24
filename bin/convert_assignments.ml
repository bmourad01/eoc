open Core_kernel

let () =
  Eoc.Parse_r.parse Sys.argv.(1)
  |> Eoc.R_typed.uniquify |> Eoc.R_typed.convert_assignments
  |> Eoc.R_typed.to_string |> print_endline

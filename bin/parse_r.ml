open Core_kernel

let () =
  Eoc.Parse_r.parse Sys.argv.(1) |> Eoc.R_typed.to_string |> print_endline

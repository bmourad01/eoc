open Core_kernel

let () =
  Eoc.Parse_r.parse Sys.argv.(1)
  |> Eoc.R_typed.uniquify |> Eoc.R_typed.interp
  |> Eoc.R_typed.string_of_answer |> print_endline

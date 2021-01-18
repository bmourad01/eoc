open Core_kernel

let () =
  let prog = Eoc.Parse_r.parse Sys.argv.(1) in
  Eoc.R.(interp prog |> string_of_answer) |> print_endline

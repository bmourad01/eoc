open Core_kernel

let () =
  let prog = Eoc.Parse_r.parse Sys.argv.(1) in
  print_endline Eoc.R_typed.(to_string (uniquify prog))

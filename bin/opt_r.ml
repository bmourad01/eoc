open Core_kernel
open Eoc

let () =
  let prog = Parse_r.parse Sys.argv.(1) in
  print_endline R.(to_string (opt prog))

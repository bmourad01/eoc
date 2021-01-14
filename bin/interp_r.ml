open Core_kernel
open Eoc

let () =
  let prog = Parse_r.parse Sys.argv.(1) in
  Printf.printf "%d\n" (R.interp prog)

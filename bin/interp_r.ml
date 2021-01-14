open Core_kernel

let () =
  let prog = Eoc.Parse_r.parse Sys.argv.(1) in
  Printf.printf "%d\n" (Eoc.R.interp prog)

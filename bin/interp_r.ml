open Core_kernel

let () =
  let prog = Eoc.Parse_r.parse Sys.argv.(1) in
  match Eoc.R.interp prog with
  | `Int i -> Printf.printf "%d\n" i
  | `Bool b -> Printf.printf "%s\n" (if b then "#t" else "#f")

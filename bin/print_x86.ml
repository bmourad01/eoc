open Core_kernel

let () =
  let p = Eoc.Parse_r.parse Sys.argv.(1) in
  let p = Eoc.R.uniquify p in
  let p = Eoc.R_anf.resolve_complex p in
  let p = Eoc.C.explicate_control p in
  let p = Eoc.X.select_instructions p in
  let p = Eoc.X.uncover_live p in
  let p = Eoc.X.build_interference p in
  let p = Eoc.X.allocate_registers p in
  let p = Eoc.X.remove_jumps p in
  let p = Eoc.X.patch_instructions p in
  print_endline (Eoc.X.to_string p)

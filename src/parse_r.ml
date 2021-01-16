open Core_kernel

let file_pos lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse filename =
  In_channel.with_file filename ~f:(fun file ->
      let lexbuf = Lexing.from_channel file in
      try R_parser.prog R_lexer.token lexbuf with
      | R_lexer.Syntax_error msg ->
          failwith
            (Printf.sprintf "R: %s lexing error: %s (%s)" filename
               (file_pos lexbuf) msg)
      | R_parser.Error ->
          failwith
            (Printf.sprintf "R: %s parse error: %s" filename
               (file_pos lexbuf))
      | Invalid_argument msg ->
          failwith
            (Printf.sprintf "R: %s invalid arg (%s): %s" filename msg
               (file_pos lexbuf))
      | _ ->
          failwith
            (Printf.sprintf "R: %s unknown parser error: %s" filename
               (file_pos lexbuf)))

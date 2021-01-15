{
  open Lexing
  open R_parser
  
  exception Syntax_error of string

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      {pos with pos_lnum= pos.pos_lnum + 1; pos_bol = pos.pos_cnum}
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z'] (alpha | '_' | '-' | '\'' | digit)*
let integer = digit+

rule token = parse
  | '\n' { newline lexbuf; token lexbuf }
  | space { token lexbuf }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LSQUARE }
  | ']' { RSQUARE } 
  | "read" { READ }
  | "let" { LET }
  | "#t" { TRUE }
  | "#f" { FALSE }
  | "eq?" { EQ }
  | '<' { LT }
  | "<=" { LE }
  | '>' { GT }
  | ">=" { GE }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "if" { IF }
  | eof { EOF }
  | integer as n { INT (int_of_string n) }
  | ident as id { VAR id }
  | _ { raise Error }

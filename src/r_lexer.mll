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
let ident = ['a'-'z'] (alpha | '-' | '\'' | digit)*
let integer = digit+

rule token = parse
  | '\n' { newline lexbuf; token lexbuf }
  | space { token lexbuf }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '/' { FSLASH }
  | "rem" { REM }
  | "land" { LAND }
  | "lor" { LOR }
  | "lxor" { LXOR }
  | "lnot" { LNOT }
  | "(define" { DEFINE }
  | "lambda:" { LAMBDA }
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
  | "set!" { SETBANG }
  | "begin" { BEGIN }
  | "while" { WHILE }
  | "vector" { VECTOR }
  | "vector-length" { VECTORLENGTH }
  | "vector-ref" { VECTORREF }
  | "vector-set!" { VECTORSETBANG }
  | "procedure-arity" { PROCEDUREARITY }
  | "void" { VOID }
  | "Integer" { TINTEGER }
  | "Boolean" { TBOOLEAN }
  | "Void" { TVOID }
  | "Vector" { TVECTOR }
  | "->" { ARROW }
  | ':' { COLON }
  | eof { EOF }
  | integer as n { INT (Int64.of_string n) }
  | ident as id { VAR id }
  | _ { raise Error }

{
open Lexing
open Ocaml

exception SyntaxError of string

let next_line lexbuf k =
  new_line lexbuf; k lexbuf

(*
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    } ; 
    k lexbuf
*) 
}


let white = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"
let letter = [ 'a'-'z' 'A'-'Z']
let non_letter = [^ ' ' '\t' '\r' '\n' '*' '(' ]
let word = letter+

let not_close = '*'+ [^ ')' '*' ]
let not_open = '('+ [^ '*' ]
           
let non_word = ( non_letter+ (not_close | not_open)? | (not_close | not_open) non_letter* )+
               

rule lex =
  parse
  | white+    { lex lexbuf }
  | newline   { next_line lexbuf lex }
  | '"''(''*'+ ')'? '"' ? { NONWORD (Lexing.lexeme lexbuf) }
  | '(' '*' ')'     { OPEN }
  | '(' '*'+ ')' { OPENCLOSE }
  | '(' '*'+  { OPEN }
  | '*'+ ')'  { CLOSE } 
  | word      { WORD (Lexing.lexeme lexbuf) }
  | non_word  { NONWORD (String.trim @@ Lexing.lexeme lexbuf) }   
  | eof       { EOF }

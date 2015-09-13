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

let rec forall s (c,e) test =
  if e<=c then true else 
  test s.[c] && forall s (c+1,e) test 

let filter_par = function
  | "()" -> NONWORD "()"
  | "(*" -> OPEN
  | "*)" -> CLOSE
  | "(*)" -> OPEN
  | s ->
    let n = String.length s in
    match n with
    | 2 | 1 | 0 -> NONWORD s
    | 3 ->
      ( match s.[0], s.[1], s.[2] with
        | '(', '*', _ -> OPEN
        | _, '*', ')' -> CLOSE
        | _ -> NONWORD s
      )
    | _ -> (
        match s.[0], s.[1], s.[n-2], s.[n-1] with
        | '(', '*', '*', ')' -> OPENCLOSE
        | _, _, '*', ')' -> CLOSE
        | '(', '*', _, _ -> OPEN  
        | _ -> NONWORD s
      )
        
    
let string_start = ref 0 
}


let white = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"

let start_operator = [ '*' '+' '%' '-' '^' '$' '@' '/' '#' '!' '=' '<' '>' '|' '&' '~' ]                     
let punctuation = [ '.' ':' ';' ',' ]
let start_leftpar = [ '(' '[' '{' ]
let end_rightpar = [ ')' ']' '}' ]                    
let in_operator = start_operator | '.'
let par =  in_operator                  
                        
let non_letter = [^ ' ' '\t' '\r' '\n' '*' '(' ]

let num = ['0' - '9' ]
let letter = [^ '_' ] # num # punctuation # start_leftpar # end_rightpar # start_operator # ['\n' '\r'] # white # [ '"' '\'' ]

let alphanum = letter | num | '_'
let alphanum_special = [ '`' '~' '?' '\'' ]
let start_alphanum = alphanum | alphanum_special


let alphanum = letter | ['0'-'9' '_']


let word = letter+

let not_close = '*'+ [^ ')' '*' ]
let not_open = '('+ [^ '*' ]
           
let non_word = ( non_letter+ (not_close | not_open)? | (not_close | not_open) non_letter* )+
               

rule lex =
               parse
             | "'\\\"'"                        { NONWORD "'\\\"'"  } 
             | '(' white+ '*' white* ')'       { NONWORD "( * )" }
             | white+                          { lex lexbuf }
             | newline                         { next_line lexbuf lex }
             | start_leftpar par*
             | par* end_rightpar          
             | start_leftpar par* end_rightpar { filter_par @@ Lexing.lexeme lexbuf }
             | start_operator in_operator*     { NONWORD (Lexing.lexeme lexbuf) }
             | punctuation+                    { NONWORD (Lexing.lexeme lexbuf) } 
             | '"'                             { string lexbuf }
             | letter+                         { WORD (Lexing.lexeme lexbuf) }
             | start_alphanum alphanum*
               { NONWORD (String.trim @@ Lexing.lexeme lexbuf) }
             | '\'' ('\\')? _ '\''             { NONWORD (Lexing.lexeme lexbuf)  }
             | '\'' '\\'  num num num '\''     { NONWORD (Lexing.lexeme lexbuf) }   
             | eof                             { EOF }

and string =
    parse
             | '"' { lex lexbuf }
             | '\\' _    {string lexbuf}
             | newline   { next_line lexbuf string }
             | [^ '"' '\\']+ { string lexbuf }

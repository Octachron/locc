%{
open Printers
let id x = x
let cons x l = x :: l  

open Ast

%}

%token OPEN
%token CLOSE
%token OPENCLOSE
%token EOF
%token <string> WORD
%token <string> NONWORD

%type <Ast.comment list> file
%start file
%%

file:
 | EOF      { [] }
 | text EOF { List.rev $1 }
;
  
text:
  | elt             { $1 [] }
  | text elt { $2 $1 }
;
elt:
  | WORD {fun l-> l }
  | NONWORD { fun l -> l }
  | comment {fun l -> $1 :: l }
;
open_close:
  | OPEN CLOSE {()}
  | OPENCLOSE {()}
comment:
  open_close {
    { text =  [] ;
      loc = { start = $startpos; end_ = $endpos }
    }

  }
| OPEN comment_text CLOSE {
  {
    text = (List.rev @@ ("*" ^ ")") :: $2 ) ;
    loc = { start = $startpos; end_ = $endpos }
  }
}
;
comment_text:
  | elt_comment                     { $1 [ "(" ^ "*" ] }
  | comment_text elt_comment   { $2 $1 }
;

elt_comment:
  | comment { fun l -> let c = $1 in c.text @ l }
  | word    { $1 }
;
word:
  | WORD { fun l -> $1 :: l }
  | NONWORD { fun l -> $1 :: l } 

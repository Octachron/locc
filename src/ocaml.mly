%{
open Printers
let id x = x

open Ast
let cons x l = x :: l 
let word_cons x l = (Word x) :: l  
let alien_cons x l = (Alien x) :: l


%}

%token OPEN
%token CLOSE
%token OPENCLOSE
%token EOF
%token <string> WORD
%token <string> NONWORD
%token STRING

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
  | CLOSE { id }
  | WORD { id }
  | NONWORD { id }
  | STRING { id }
  | comment { cons $1 }
;
open_close:
  | OPEN CLOSE {()}
  | OPENCLOSE {()}
comment:
  open_close {
    { text =  [] ;
      loc = (
      Loc.create_conv [ $startpos; $endpos ]
    )
    }

  }
| OPEN comment_text CLOSE {
  {
    text = (List.rev  $2 ) ;
    loc = (
      Loc.create_conv [ $startpos; $endpos ]
    )
  }
  }

;
comment_text:
  | elt_comment                     { $1 [] }
  | comment_text elt_comment   { $2 $1 }
;

elt_comment:
  | comment { fun l -> let c = $1 in c.text @ l }
  | word    { $1 }
;
word:
  | STRING { id }
  | WORD { word_cons $1 }
  | NONWORD { alien_cons $1 } 
;

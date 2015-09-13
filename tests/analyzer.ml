
open Ast

open Printers
    
let comments = Ocaml.file Ocaml_lex.lex @@ Lexing.from_string "(* print_string ( ^ n ^ ) ; print_newline () ; 
 *)"

let pp_int ppf = fpp ppf "%d"

let analyze ppf word =
  let kind = Model.classify_word Model.(start_score fr_en) word in
  fpp ppf "%a [%a] " Ast.Pp.word word Pp.(list ~sep:(const "|") pp_int) kind

let analyze_comment ppf c =
 fpp ppf "%a" (Pp.list analyze) c.text

let analyze ppf =
  fpp ppf "%a" (Pp.list analyze_comment)

let () = analyze stdout comments

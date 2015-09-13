open Ocaml
open Printers

let show_token =
  let open Ocaml in
  function
  | OPEN -> "\x1b[34m(*"
  | CLOSE -> "*)\x1b[39m"
  | OPENCLOSE -> "\x1b[34m(**)\x1b[39m"
  | NONWORD s ->  "⟨⟨" ^ s ^ "⟩⟩ "
  | WORD s ->  "⟨" ^ s ^ "⟩"
  | EOF -> "\n"
  | STRING -> {| ".." |}

let lexbuff =
  Lexing.from_channel @@ open_in "src/alien.ml"
(* Lexing.from_string "hi" *)

let rec loop lexbuf =
  let token = Ocaml_lex.lex lexbuf in
  Printf.printf "%s " (show_token token );
  if token = Ocaml.EOF then () else loop lexbuf

let () = loop lexbuff

let () =
  Pp.header stdout "Lexing ocaml.ml" loop
    (Lexing.from_channel @@  open_in "tests/lex_test.txt")

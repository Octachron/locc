let () =
  let lexbuf = Lexing.from_channel @@ open_in "tests/lex_test.txt" in
  let comments = Ocaml.file Ocaml_lex.lex lexbuf in
  Ast.Pp.comments Printers.stdout comments

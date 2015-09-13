open Ast
open Printers

let path = Loc.split_filename "./file.ml"

let () =
  fpp stdout "%a\n" (Pp.list Loc.Pp.atom) path

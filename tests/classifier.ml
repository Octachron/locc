open Report
open Crawler

let update_memory (alien,memory) (c:Ast.comment) =
  let open Ast in
  let open Comment in
  let kind = Model.(classify fr_en) c.text in
  if Model.(exactly alien kind) then
  try
        let info = Hashtbl.find memory c.text in
        Hashtbl.replace memory c.text { info with locs = c.loc :: info.locs }
  with Not_found ->
        Hashtbl.add memory c.text { choice = kind; text = c.text; locs = [c.loc] }



let action env f =
  let chan = open_in f in
  let lexbuf () = Lexing.from_channel chan in
  (*  loop @@ lexbuf (); *)
  ( try let comments = Ocaml.file Ocaml_lex.lex @@ lexbuf () in
    List.iter (update_memory env) comments
    with
    | Ocaml.Error  -> ( print_newline (); Printf.printf "Parser error: %s \n" f
                        (* loop @@ lexbuf () *) )
    | Failure s -> ( print_newline(); Printf.printf "Lexer error: %s, %s \n" s f
                     (* loop @@ lexbuf () *) )
  )
; close_in chan


open Printers

let () =
  let memory = Hashtbl.create 10 in
  let print src =
    action (Model.alien,memory) src;
    Hashtbl.iter (fun k x -> Comment.Pp.info stdout x) memory in   
  Pp.header stdout "Classification 1" print "src/alien.ml"
let () =
  let memory = Hashtbl.create 1000 in
  let print context =
    process "french" context @@ action (Model.alien,memory);
    Log.with_ "french2" @@ fun log ->
    let open Comment in
    Hashtbl.iter (fun k x ->
        if List.length x.locs < 5 then
          Pp.info (formatter log) x
      )
      memory in
  Pp.header stdout "Classification 2" print "."

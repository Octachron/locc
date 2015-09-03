

type loc = { start : Lexing.position; end_: Lexing.position }

type comment = { text: string list; loc : loc }

type ast = comment list


let pp_list pp ppf= Printers.pp_list pp ppf


let col p = Lexing.( p.pos_cnum - p.pos_bol )

let pp_position ppf p =
  let open Lexing in
  Printf.fprintf ppf "l%d.%d" p.pos_lnum @@ col p 

let pp_loc ppf loc =
  let open Lexing in
  if loc.start.pos_lnum <> loc.end_.pos_lnum then
  Printf.fprintf ppf "%a — %a"
    pp_position loc.start
    pp_position loc.end_
  else
    Printf.fprintf ppf "l%d.(%d — %d)"
      loc.start.pos_lnum
      (col loc.start) (col loc.end_) 

let pp_comment ppf c =
  Printf.fprintf ppf " %a \n %a "
    pp_loc c.loc
    ( pp_list (fun ppf -> Printf.fprintf ppf "%s" ) ) c.text
; Printf.fprintf ppf "\n"
    
let pp_comments ppf = pp_list pp_comment ppf


open FileUtil
open FilePath
open Ast

type context = { pwd : filename }

let src_filter =
  let ( ^ ) x y= Or(x,y) in
  List.fold_left ( fun c x -> c ^ Has_extension x)  (Has_extension "ml") ["mli";"mll";"mly"]


let hidden d =
  d |> basename |> chop_extension <> ""

let dir_filter = And( Is_dir, Custom hidden ) 

let rec process context action =
  let children = ls context.pwd in
  let files = filter src_filter children in
  let grandchildren = filter dir_filter children in
  List.iter (fun f -> Printf.printf "Source file: %s \n" f; action f) files;
  List.iter (Printf.printf "Directories: %s \n" ) grandchildren;
  List.iter (fun pwd -> process { pwd } action ) grandchildren 

             
let () =
  process { pwd = "." } (fun _ -> () )

let show_token =
  let open Ocaml in
  function
  | OPEN -> "\x1b[34m(*"
  | CLOSE -> "*)\x1b[39m"
  | OPENCLOSE -> "\x1b[34m(**)\x1b[39m"
  | NONWORD s ->  "⟨⟨" ^ s ^ "⟩⟩ "
  | WORD s ->  "⟨" ^ s ^ "⟩"
  | EOF -> "\n"
                 


let lexbuff =
  Lexing.from_channel @@ open_in "src/alien.ml"
(* Lexing.from_string "hi" *)

let rec loop lexbuf =
  let token = Ocaml_lex.lex lexbuf in
  Printf.printf "%s " (show_token @@ token );
  if token = Ocaml.EOF then () else loop lexbuf

let () = loop lexbuff

let () =
  let lexbuf = Lexing.from_channel @@ open_in "src/alien.ml" in
  let comments = Ocaml.file Ocaml_lex.lex lexbuf in
  pp_comments stdout comments


type hypothesis = string

type model = { h: hypothesis; dict: Aspell.Speller.t; llh : float * float }
type estimator = { model : model; score:float }

let create model = { model; score = 0. }

type result = Positive | Negative
let update e r =
  let pos, neg = e.model.llh in
  let up = match r with Positive -> pos | Negative -> neg in
  { e with score = e.score +. up }
                  
type classifier = estimator * estimator

type decision = float * hypothesis
let pp_decision ppf (p,x) =
  Printf.fprintf ppf "%s:%f" x (exp p)

type choice = Either of decision * decision | Clearly of decision

let mem kind = function
  | Clearly (p,c) -> kind = c
  | Either ( (p,c), (p2,c2) ) -> (kind = c) || (kind = c2)  

let exactly kind = function
  | Clearly (p,c) -> kind = c
  | Either _ -> false

let pp_choice ppf = function
  | Clearly d -> Printf.fprintf ppf "[%a]" pp_decision d
  | Either (d1,d2) -> Printf.fprintf ppf "[%a|%a]" pp_decision d1 pp_decision d2

let extract_decision e = e.score, e.model.h

let choose (e1,e2) =
  let extr = extract_decision in
  let diff = e1.score -. e2.score in
  match abs_float diff > log 2., diff > 0. with
  | false, _ -> Either (extr e1, extr e2)
  | true, true -> Clearly (extr e1)
  | true, false -> Clearly (extr e2)
                     
let classify_word (e1,e2) word =
  let open Aspell.Speller in
  match check e1.model.dict word, check e2.model.dict word with
  | Some true, Some true -> update e1 Positive, update e2 Positive 
  | Some true, _ -> update e1 Positive, update e2 Negative
  | _ , Some true -> update e1 Negative, update e2 Positive
  | _, _ -> (e1,e2)

let classify (m1,m2) comment =
  comment
  |> List.fold_left classify_word (create m1, create m2)
  |> choose


exception Aspell_msg of string

let dict name =
  let conf = Aspell.Config.create () in
  let _ = Aspell.Config.replace conf "lang" name in
  let sp_opt = Aspell.Speller.create conf in
  match sp_opt with
  | `Error msg-> raise @@ Aspell_msg msg
  | `Ok d -> d

let llh (self,alien) = log self, log alien

let model ~name ~density = {
  h = name;
  dict = dict name;
  llh = llh density
}

let alien = "fr_FR-80"

let french = model ~name:alien ~density:(0.9,0.1)
let english = model ~name:"en_US-80" ~density:(0.9,0.1)

type comment_info = {
  choice: choice;
  text: string list;
  locs : loc list
}

let update_memory (alien,memory) (c:comment) =
  let kind = classify (french,english) c.text in
  if exactly alien kind then
  try
        let info = Hashtbl.find memory c.text in
        Hashtbl.replace memory c.text { info with locs = c.loc :: info.locs }
  with Not_found ->
        Hashtbl.add memory c.text { choice = kind; text = c.text; locs = [c.loc] }

let pp_word ppf x = Printf.fprintf ppf "%s" x

let pp_comment_info ppf c =
    Printf.fprintf ppf "comment: %a\n locations: %a \n %a \n"
      pp_choice c.choice
      (pp_list pp_loc) c.locs 
      (pp_list pp_word) c.text



let action env f =
  let chan = open_in f in
  let lexbuf () = Lexing.from_channel chan in
  (*  loop @@ lexbuf (); *)
  ( try let comments = Ocaml.file Ocaml_lex.lex @@ lexbuf () in
    List.iter (update_memory env) comments
  with Ocaml.Error -> Printf.printf "Parser error\n"
  )
; close_in chan

let header action arg= 
  Printf.printf "\n-------------Classification----------\n";
  action arg;
  Printf.printf   "-------------------------------------\n"
  
let () =
  let memory = Hashtbl.create 10 in
  let print src =
    action (alien,memory) src;
    Hashtbl.iter (fun k x -> pp_comment_info stdout x) memory in   
  header print "src/alien.ml"


let () =
  let memory = Hashtbl.create 1000 in
  let print context =
    process context @@ action (alien,memory);
    Hashtbl.iter (fun k x -> if List.length x.locs = 1 then  pp_comment_info stdout x) memory in
 header print { pwd = "." }

(* Un commentaire français *)
(* Un second commentaire *)
(* Un commentaire français *)
(* Hi ? *)

(* It this really possible? *)
(** Documentation comment *)

(**/**)

(*) *)

(************)

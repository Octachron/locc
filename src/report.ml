open Printers

type parameters = { log : string; model: Model.t } 
let default = { log = "log"; model=Model.fr_en }
let create_par log model = {log; model }

type context = { dir : FilePath.filename; analysis: Comment.analysis }

module Resume = struct
  type t = int Maps.Cl.t
  let empty = Maps.Cl.empty
  let add m key =
    let n = match Maps.Cl.find key m with
      | n -> 1 + n
      | exception Not_found -> 1 in
    Maps.Cl.add key n m
  let (|+>) = add
end

module Log = struct
  let name {log} s = Printf.sprintf "%s/%s" log s

  let create ?(p=default) s = open_out @@ name p s

  let with_ ?(p=default) name f =
    let log = create ~p name in
    let r = f log in
    flush log;
    close_out log; r
end


module Files = struct
  open FileUtil
  open FilePath

  let not_hidden d =
    d |> basename |> chop_extension <> ""
                     
  
  module Filter = struct
    let src =
  let ( ^ ) x y= Or(x,y) in
  List.fold_left ( fun c x -> c ^ Has_extension x)  (Has_extension "ml") ["mli";"mll";"mly"]

    let dir = And( Is_dir, Custom not_hidden ) 
  end
end

module Parser = struct

  let from_file filename =
    let f = open_in filename in
    let lexbuf = Lexing.from_channel f in
    let open Lexing in
    lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_fname = filename };
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    f, lexbuf
    
  exception Error
  let comments filename =
    let f, lexbuf = from_file filename in
    try
      let r = Ocaml.file Ocaml_lex.lex lexbuf  in
      close_in f;
      r
    with
    | Ocaml.Error -> close_in f; raise Error

end

let rec analyze context =
  let children = FileUtil.ls context.dir in
  let leaves = FileUtil.filter Files.Filter.src children in
  let nodes = FileUtil.filter Files.Filter.dir children in
  let file_analyzer analysis s =
    try List.fold_left Comment.add analysis @@ Parser.comments s
    with
    | Parser.Error ->
      fpp stdout "Parser error: %s\n" s;
      analysis
  in
  let analysis =
    List.fold_left file_analyzer context.analysis leaves in
  let dir_folder context dir =
     analyze { context with dir } in
  List.fold_left dir_folder { context with analysis } nodes


let analyze {model; _} dir =
  let analysis = Comment.{ model; store=empty } in
  let context = { dir ; analysis } in  
  (analyze context).analysis


    

let create params analysis =
  let keyname = List.map (fun x -> x.Model.text) in
  let bindings = Maps.Cl.bindings @@ Comment.split analysis in
  
  let open_log suffix (key:Model.id_name list) =
    let add_suffix l = match suffix with
      | Some x -> l @ [x]
      | None -> l in
    let keyname = add_suffix @@ keyname key in  
    let () = fpp str "%a" Pp.(list ~sep:(const "+") string) keyname in
    Printers.get_str ()
  |> Log.create ~p:params in
  let log_mult (key,{Comment.mult; unique}) =
    let out = open_log (Some "mult") key in
    List.iter (fpp (formatter out) "%a\n" Comment.Pp.info) mult
  ; close_out out in
  let log_sing (key, {Comment.unique; mult }) =
    let out = open_log None key in 
    let tree = Comment.Tree.create unique in
    fpp (formatter out) "%a" Comment.Pp.tree tree
  ; close_out out in
  let log cs =
    log_mult cs;
    log_sing cs in
  let cl_report (key,{Comment.unique; mult} ) =
    fpp stdout "Class [%a] : %n singular, %n multiple\n"
      Pp.(list ~sep:(const "|") string) (keyname key)
      (List.length unique) (List.length mult) in
  let iteration kv =
    log kv; cl_report kv in
  List.iter iteration bindings

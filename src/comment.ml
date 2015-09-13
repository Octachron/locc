open Printers

module Map =
  Map.Make( struct
    type t = Ast.word list
    let compare (x:t) (y:t) = compare x y
  end)
    
type 'loc gen_info = {
  choice: Model.choice;
  text: Ast.word list;
  locs : 'loc
}

type info = Ast.Loc.t list gen_info
type sing_info = Ast.Loc.t gen_info

let rec extract_file_loc = function
  | [] -> [], []
  | a::q as l-> match a with
    | Ast.Loc.Dir d -> let file, nonfile = extract_file_loc q in a :: file, nonfile
    | Ast.Loc.File f -> [a], q
    | _ -> [], l

let factor_file_loc x =
  let open Ast in
  let factor, left = extract_file_loc x.left in
  factor, { x with left }

let singularize info =
  match info.locs with
  | [locs] -> let factor, locs = factor_file_loc locs in
    factor, { info with locs }
  | _ -> assert false

type analysis = { model : Model.t ; store: info Map.t }

let empty = Map.empty

let add ( {store;model} as analysis ) (comment:Ast.comment) =
  let open Ast in
  let info = match Map.find comment.text store with
    | exception Not_found ->
      {
        choice = Model.classify model comment.text
      ; text = comment.text
      ; locs = [ comment.loc ]
      }
    | info -> { info with locs = comment.loc :: info.locs } in
  { analysis with store = Map.add comment.text info store }

let (|+>) = add

type split_list = { mult : info list; unique : info list }

let split { model; store } =
  let bindings = Map.bindings store in
  let add m (_,info) =
    let key = Model.classification info.choice in
    let cons info sl =
      if List.length info.locs > 1 then
        { sl with mult = info :: sl.mult}
      else
        { sl with unique = info::sl.unique } in
    let infos = match Maps.Cl.find key m with
      | infos -> infos
      | exception Not_found -> { mult = []; unique = []} in
    Maps.Cl.add key (cons info infos) m in
  List.fold_left add Maps.Cl.empty bindings


module Tree = struct
type t =
  | Leaf of sing_info list
  | Node of t Maps.Loc.t

let rec create_path path comment =
  match path with
  | [] -> Leaf [comment]
  | atom :: q ->
    let branch = Maps.Loc.add atom (create_path q comment) Maps.Loc.empty in
    Node branch

exception Path_incompatibility of string

let rec add_at_path tree path comment =
  match tree, path with
  | Leaf l , [] -> Leaf (comment::l)
  | Node m, atom::q ->
    begin
      let branch = 
        try
          let branch = Maps.Loc.find atom m in
          add_at_path branch q comment
        with
        | Not_found ->
          create_path q comment
      in
      Node ( Maps.Loc.add atom branch m )
    end
  | Leaf _, _::_ -> raise @@ Path_incompatibility "Leaf"
  | Node _, [] -> raise @@ Path_incompatibility "Node"

let add tree comment =
  let path, comment = singularize comment in
  add_at_path tree path comment

let root = Node Maps.Loc.empty

      
let create =
  List.fold_left add root

let rec ellide= function
  | Node m as t->
    (
    match Maps.Loc.bindings m with
    |  [k , t ] ->
      let factor, tree = ellide t in
      k :: factor , tree
    | _ -> [], t
    )      
  | t -> [], t 
  


  
end



module Pp = struct
  let info_gen ~pp_loc ppf c =
    fpp ppf "@[\x1b[1mcomment %a %a\x1b[0m@;%a@]\n"
      pp_loc c.locs 
      Model.Pp.choice c.choice
      (Pp.list Ast.Pp.word) c.text
      
  let info = info_gen ~pp_loc:(Pp.list Ast.Loc.pp)
  let sing = info_gen ~pp_loc:Ast.Loc.pp

  
  let rec tree ppf =
    let open Tree in
    function
  | Leaf l -> fpp ppf "@[<2>%a@]"
                Pp.(list ~sep:(const "\n") sing )
                (List.sort
                   ( fun x y -> Ast.Loc.compare_factorized x.locs y.locs )
                   l)
  | Node m ->
    let b = Maps.Loc.bindings m in
    fpp ppf "@[<v 2>%a@]" Pp.( list ~sep:(const "@;") @@
                  fun ppf (k,v) -> fpp ppf "@[<v 2>@[<2>\x1b[4m%a\x1b[0m@]@;%a@]"
                    Ast.Loc.Pp.data k
                    tree v
                )
      b

let tree ppf t =
  let factor, t = Tree.ellide t in
  fpp ppf "@[\x1b[4m%a\x1b[0m@]@." Pp.(list Ast.Loc.Pp.data) factor;
  tree ppf t
end

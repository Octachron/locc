
open Printers

type 't factorized =
  { left: 't; right: 't ; center: 't list }

module Loc = struct

  type atom =
    | Dir of string
    | File of string
    | Line of int
    | Col of int


  
  type partial = atom list
      
  let compare x y = match x, y with
    | Dir _, ( File _ | Line _ | Col _ ) -> 1 
    | File _, (Line _ | Col _ ) -> 1
    | Line _, Col _ -> 1
    | File a, File b | Dir a, Dir b -> compare a b
    |Line a, Line b | Col a, Col b -> compare a b
    | ( Col _ | Line _ | File _ ), Dir _ -> -1                       
    | ( Col _ | Line _ ) , File _ -> -1
    | Col _ , Line _ -> -1


  let rec compare_partial x y = match x, y with
    | [], [] -> 0
    | a::q, [] -> -1
    | [], a::q -> 1
    | a::q, b::r ->
      let c = compare a b in
      if c = 0 then
        compare_partial q r
      else
        c

  let compare_factorized x y = compare_partial x.left y.left
        
  let split_filename s=
    let n = String.length s in
    let sub start end_ =
        String.sub s start (end_ - start) in
    let rec split start end_ =
      match s.[end_], end_ = n -1 with
      | '/', false -> Dir(sub start end_) :: split ( end_ + 1 ) ( end_ + 1 )
      | '/' , true -> [ Dir (sub start end_) ]
      | _ , true -> [ File( sub start (end_+1) ) ]
      | _, false -> split start (end_+1) in
    if n > 1 then
      split 0 0
    else
      [File s]
  
let loc_col p = Lexing.( p.pos_cnum - p.pos_bol )
let loc_line p = Lexing.(p.pos_lnum)
let loc_file p =
  let s = Lexing.(p.pos_fname) in
  split_filename s

let convert pos =
  loc_file pos @ Lexing.[ Line (loc_line pos ); Col (loc_col pos) ]

module Broken_invariant =
struct
  exception Nonordered
  exception Nontotal
end

let rec equals = function
  | [] -> true
  | [a] -> true
  | a::(b::q as r) -> a=b && equals r

let advance = function
  | [] -> raise Broken_invariant.Nontotal
  | a :: q -> a, q

let create loc =
  let rec left_factor factors = function
    | (a::q)::r as l ->
      let hds, tails = List.split @@ List.map advance l in
      begin
        if equals hds then
          left_factor (a::factors) tails
        else
          List.rev factors, l
      end
    | []::r as l ->
      List.rev factors,
      if equals l then
        l
      else raise Broken_invariant.Nontotal
    | _ -> raise Broken_invariant.Nontotal
   in
  let left, center = left_factor [] loc in
  let right, center = left_factor [] @@ List.map List.rev center in
  let center = List.map List.rev center in
  { left; center; right }

let create_conv l = l |> List.map convert |> create

type t = partial factorized

module Pp = struct
let atom ppf = function
  | Dir d -> fpp ppf "Dir[%s]" d
  | File f -> fpp ppf "File[%s]" f
  | Line l -> fpp ppf "Line[%d]" l
  | Col c -> fpp ppf "Col[%d]" c

let data ppf = function
  | Dir d -> fpp ppf "%s/" d
  | File f -> fpp ppf "%s:" f
  | Line l -> fpp ppf "l%d." l
  | Col c -> fpp ppf "%d" c

let partial ppf p =
  fpp ppf "%a" Pp.(list ~sep:(const "") data) p

let loc ppf loc =
  let p = partial in
  fpp ppf "@[%a(%a)%a@]"
    p loc.left
    Pp.(list ~sep:(const " — ") p) loc.center 
    p loc.right
end
let pp = Pp.loc

end

type parser_loc = { start : Lexing.position; end_: Lexing.position }

type word =
  | Word of string
  | Alien of string

type comment = { text: word list; loc : Loc.t }

type ast = comment list



module Pp = struct
let word ppf = function
  | Word s -> Pp.string ppf s
  | Alien s -> fpp ppf "%s" s 


let comment ppf c =
  fpp ppf "@[<2>%a\n%a@]@;"
    Loc.pp c.loc
    Pp.( list word ) c.text

    
let comments ppf = Pp.list comment ppf
end

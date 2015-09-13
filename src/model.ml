open Printers

type name = string
type id_name = { text : name ; id : int } 

type hypothesis = { name: id_name ; test: string -> bool; prob : float array }
type estimator = { h : hypothesis; score:float }
type t = hypothesis array            
type estimation = estimator array

                  
type classifier = estimator array
type classification = id_name list

type decision = {score: float; name:id_name}
type choice = decision list


let unknown m = Array.length m - 1 

let start_estimation h = { h; score = 0. }

let update result e =
  let score_diff =
    List.fold_left ( fun score i -> score +. e.h.prob.(i) ) 0. result in
  { e with score = e.score +. log score_diff }


let rec mem kind = function
  | [] -> false
  | { score; name }  :: q  -> (kind = name.text) || mem kind q  

let exactly kind = function
  | [{ score; name }] -> kind = name.text
  |  _ -> false


let extract_decision (e:estimator) : decision = {score = e.score; name = e.h.name}

let best_decision =
  let rec max m = function
    | [] -> m
    | a::q -> max (if a.score > m.score then a else m) q in
  function
  | [] -> assert false
  | a::q -> max a q 

let choose decisions =
  let dlist = List.map extract_decision @@ Array.to_list decisions in
  let m = best_decision dlist in
  let test x = m.score -. x.score < log 2. in
  List.filter test dlist

let classification = List.map (fun x ->  x.name)

let classify_word model word =
  let folder word (i,l) x =
    match x.h.test word with
    | true -> i+1, i::l
    | _ -> i+1, l in
  let add_alien = function
    | [] -> [unknown model]
    | l -> l in
  match word with
  | Ast.Alien _ -> [unknown model]
  | Ast.Word s -> 
    let _, classified = Array.fold_left (folder s) (0,[]) model in
    add_alien classified

let score_word model word =
  let classified = classify_word model word in
  Array.map (update classified) model

let start_score model = Array.map start_estimation model

let classify hs comment =
  comment
  |> List.fold_left score_word @@ start_score hs
  |> choose


exception Aspell_msg of string

let dict name =
  let conf = Aspell.Config.create () in
  let _ = Aspell.Config.replace conf "lang" name in
  let sp_opt = Aspell.Speller.create conf in
  match sp_opt with
  | `Error msg-> raise @@ Aspell_msg msg
  | `Ok d -> fun s ->
    match Aspell.Speller.check d s with
    | Some true -> true
    | None | Some false -> false

let alien_prob n =
  let self = 0.9 in
  Array.init (n+1) (fun k ->
      if k < n then
        (1. -. self) /. float_of_int n
      else
        self
    )


type error =
  | Incomplete_hypothesis of name
  | Nonnormalized_hypothesis of name * float


type check =
  | Ok
  | Error of error list



let (&&&) check1 check2 =
  match check1 with
  | Ok -> check2
  | Error l -> match check2 with
    | Error l' -> Error (l @ l')
    | Ok -> Error l

let check_len expected (name,a) =
  let m = Array.length a in
  if m = expected then
    Ok
  else
    Error [Incomplete_hypothesis name]

let check_normalization ?(eps=1e-10) (name,line) =
  let norm = Array.fold_left (+.) 0. line in
  if abs_float ( 1. -. norm  ) < eps
  then
    Ok
  else
    Error [Nonnormalized_hypothesis (name,norm)]
      

let check_matrix ?(eps=1e-10) m =
  let len x = Array.length x in
  let n = len m in
  let check_line line =
    check_len (n+1) line
    &&& check_normalization line in
  Array.fold_left (fun t l -> t &&& check_line l) Ok m


exception Invalid_matrix of error list

let create matrix =
  match check_matrix matrix with
  | Error l -> raise (Invalid_matrix l)
  | Ok -> 
    let n = Array.length matrix in
    Array.init (n+1) ( fun i ->
        if i < n then
          let text, prob = matrix.(i) in
          let name = { text ; id = i } in
          { name; test = dict text; prob }
        else
          {name = { text = "alien"; id = n + 1 } ; test = (fun s -> false) ; prob = alien_prob n } 
          
    )

let alien = "fr"

let fr_en = create
    [|
      "fr", [| 0.6; 0.1; 0.3 |];
      "en",  [| 0.1; 0.6; 0.3 |]
    |] 


module Pp = struct

let decision ppf { score; name} =
  fpp ppf "%s:%f" name.text (exp score)

  
let choice ppf l =
  fpp ppf "[%a]"
    Pp.(list ~sep:(const "|") decision) l

let error ppf = function
    | Incomplete_hypothesis name ->
      fpp ppf "\t Incomplete hypothesis: %s" name
    | Nonnormalized_hypothesis (name,f) ->
      fpp ppf "\t Nonnormalized hypothesis: %s:%f" name f

end

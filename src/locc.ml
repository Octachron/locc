
open Printers

open Cmdliner
let start =
  let doc = "The directory to analyze." in
  Arg.( value & pos 0 string "." & info [] ~docv:"INPUT" ~doc )

let model =
  let doc = "Statistical language model used for comment classification (default: 
fr:0.8 0.15 0.05 %en:0.15 0.8 0.05  )" in
  let info_v = Arg.info ["m";"model"] ~docv:"MODEL" ~doc in
  let open Arg in
  let m = array ~sep:'%' @@ pair ~sep:':' string @@ array ~sep:' ' float in
  let opt = opt (some m) None in
  value & opt & info_v

let lift_model = Term.pure @@ function
  | None -> Model.fr_en
  | Some m -> try Model.create m with
    | Model.Invalid_matrix l ->
      begin
      fpp stderr  "Invalid model:\n%a\n"
        Pp.(list ~sep:(const "\n") Model.Pp.error) l ; 
      exit 1
      end
      
let log =
  let doc = "Output directory" in
  Arg.( value & opt string "log" & info ["o"; "output"] ~docv:"OUTPUT" ~doc )

let term =
  let param = Term.( pure Report.create_par $ log $ ( lift_model $ model ) ) in
  let f param start =  Report.create param @@ Report.analyze param start in
  Term.( pure f $ param $ start)

let info =
  let doc = "Analyze ocaml source code to classify comment languages"
  and man = [] in
  Term.info "locc" ~version:"0.1" ~doc ~man

let () =
  match Term.eval (term,info) with
  | `Error _ -> exit 1
  | `Ok () | `Version | `Help -> exit 0


(* Un commentaire français *)
(* Un second commentaire *)
(* Un commentaire français *)
(* Hi ? *)

(* It this really possible? *)
(** Documentation comment *)

(**/**)

(*) *)

(************)

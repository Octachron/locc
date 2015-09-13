open Report
open FilePath
open FileUtil

type context = { pwd : filename; log: out_channel }

let rec process context action =
  let children = ls context.pwd in
  let files = filter Files.Filter.src children in
  let grandchildren = filter Files.Filter.dir children in
  List.iter (fun f -> Printf.printf "Source file: %s \n" f; action f) files;
  List.iter (Printf.printf "Directories: %s \n" ) grandchildren;
  List.iter (fun pwd -> process { context with pwd } action ) grandchildren 


let process name pwd action =
  let log = Log.create name in
  process { pwd; log } action;
 close_out log


let () =
  process "void" "." (fun _ -> () )


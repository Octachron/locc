

let fpp = Format.fprintf
let spp = fun () -> Printf.sprintf

let stdout = Format.std_formatter
let stderr = Format.err_formatter
let str = Format.str_formatter
let get_str () =
  Format.flush_str_formatter ()


let formatter out = Format.formatter_of_out_channel out

module Pp = struct
let rec repeat n pp ppf x =
  if n <= 1 then
    pp ppf x
  else
    fpp ppf "%a%a" pp x (repeat (n-1) pp) x 

  

let const s ppf () = fpp ppf s

let line ?(len=80) ppf =
 fpp ppf "\n%a"
 (repeat len (const "-"))

let rec list
    ?(empty=fun ppf () -> fpp ppf "")
    ?(sep=fun ppf () -> fpp ppf " ")
    pp ppf = function
  | [] -> empty ppf ()
  | [a] -> pp ppf a
  | a::q -> fpp ppf "%a%a%a"
              pp a
              sep ()
              (list ~empty ~sep pp) q
              
let string ppf s = fpp ppf "%s" s

let header ppf name action arg =
  let n = String.length name in
  let h = (80 - n) /2 in
  fpp ppf "\n%a%s%a\n"
      (line ~len:h) ()
      name
      (line ~len:h) ()
  ; action arg
  ; line ~len:(2*h+n) ppf ()
  let pp = fpp

end

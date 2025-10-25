let rec last_two (xs: 'a list): ('a * 'a) option =
  match xs with
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: tail -> last_two tail;;

(* 

rlwrap ocaml

#use "./02_last_two.ml";;

last_two ["a"; "b"; "c"; "d"];;

*)

let rec duplicate (xs: 'a list): 'a list =
  match xs with
  | [] -> []
  | h :: t -> h :: h :: duplicate t;;

(* 

rlwrap ocaml

#use "./14_duplicate.ml";;

duplicate ["a"; "b"; "b"; "a"];;

duplicate ["a"; "b"; "c"; "c"; "d"];;

*)

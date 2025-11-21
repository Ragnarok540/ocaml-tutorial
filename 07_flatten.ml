type 'a node =
  | One of 'a 
  | Many of 'a node list

let rec flatten (xs: 'a node list): 'a list =
  match xs with
  | [] -> []
  | h :: t ->
    match h with
    | One h -> h :: (flatten t)
    | Many h -> (flatten h) @ (flatten t);;

(* 

rlwrap ocaml

#use "./07_flatten.ml";;

flatten [One "a"; One "e"];;

flatten [Many [One "a"; One "e"]];;

flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;

*)

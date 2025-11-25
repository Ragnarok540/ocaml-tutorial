#use "./17_split.ml";;

let rotate (xs: 'a list) (n: int): 'a list =
  let x, y = split xs n in
  y @ x;;

(* 

rlwrap ocaml

#use "./19_rotate.ml";;

rotate ["a"; "b"; "b"; "a"] 2;;

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;

*)

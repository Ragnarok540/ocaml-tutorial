#use "./17_split.ml";;

let slice (xs: 'a list) (i: int) (k: int): 'a list =
  start (List.rev (start (List.rev xs) (List.length xs - i) 0)) k 0;;

(* 

rlwrap ocaml

#use "./18_slice.ml";;

slice ["a"; "b"; "b"; "a"] 1 2;;

slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;

*)

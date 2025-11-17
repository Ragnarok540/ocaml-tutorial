let rec compress (xs: 'a list): 'a list =
  match xs with
  | [] -> []
  | [a] -> [a]
  | a :: b :: t ->
    if a = b then
      compress (b :: t)
    else
      a :: compress (b :: t);;

(* 

rlwrap ocaml

#use "./08_compress.ml";;

compress ["a"; "b"; "b"; "a"];;

compress ["a"; "b"; "c"; "d"];;

compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

*)

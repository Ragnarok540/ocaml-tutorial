let rec replicate (xs: 'a list) (n: int): 'a list =
  let rec replicate_aux (x: 'a) (n: int) (acc: 'a list): 'a list =
    if n == 0 then
      acc
    else
      x :: replicate_aux x (n - 1) acc
  in
  match xs with
  | [] -> []
  | h :: t -> replicate_aux h n [] @ replicate t n;;

(* 

rlwrap ocaml

#use "./15_replicate.ml";;

replicate ["a"; "b";] 3;;

replicate ["a"; "b"; "c";] 2;;

*)

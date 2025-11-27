let rand_elem (xs: 'a list): 'a =
  List.nth xs (Random.int (List.length xs));;

let rec rand_select (xs: 'a list) (n: int): 'a list =
  if n = 0 then
    []
  else
    (rand_elem xs) :: rand_select xs (n - 1);;

(* 

rlwrap ocaml

#use "./23_rand_select.ml";;

rand_elem ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"];;

rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;

*)

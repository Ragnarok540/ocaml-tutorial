#use "./24_lotto_select.ml";;

let rec permutation (xs: 'a list): 'a list =
  if List.length xs = 0 then
    []
  else
    let ls, i = remove_rand xs in
    i :: permutation ls;;

(* 

rlwrap ocaml

#use "./25_permutation.ml";;

permutation ["a"; "b"; "c"; "d"; "e"; "f"];;

*)

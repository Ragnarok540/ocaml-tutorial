#use "./10_encode.ml";;
#use "./33_factors.ml";;

let factors2 (n: int): (int * int) list =
  n |> factors |> encode2;;

(* 

rlwrap ocaml

#use "./34_factors2.ml";;

factors2 315;;

factors2 864;;

*)

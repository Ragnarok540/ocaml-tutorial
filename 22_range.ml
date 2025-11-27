let rec range_up (i: int) (j: int): int list =
  if i = j then
    [i]
  else
    i :: range_up (i + 1) j;;

let range (i: int) (j: int): int list =
    if i < j then
      range_up i j
    else
      List.rev (range_up j i);;

(* 

rlwrap ocaml

#use "./22_range.ml";;

range 4 9;;

range 9 4;;

range 5 ~-5;;

*)

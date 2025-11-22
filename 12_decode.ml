type 'a rle =
  | One of 'a
  | Many of int * 'a

let rec decode (xs: 'a rle list): 'a list =
  match xs with
  | [] -> []
  | h :: t ->
    match h with
    | One h -> h :: (decode t)
    | Many (x, y) when x == 1 -> y :: (decode t)
    | Many (x, y) when x > 1 -> y :: decode ([Many (x - 1, y)]) @ decode t;;

(* 

rlwrap ocaml

#use "./12_decode.ml";;

decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;

*)

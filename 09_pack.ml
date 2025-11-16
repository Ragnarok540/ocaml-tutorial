#use "./05_rev.ml";;

let rec pack_helper (xs, acc: 'a list * 'a list): 'a list list =
  match xs with
  | [] -> [[]]
  | [a] -> [[a]]
  | a :: b :: t ->
    if a = b then
      pack_helper (b :: t, a :: acc) 
    else
      [a :: acc; b :: t];;

let pack (xs: 'a list): 'a list list =
  let rec aux current acc = function
    | [] -> []
    | [x] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
      if a = b then
        aux (a :: current) acc t
      else
        aux [] ((a :: current) :: acc) t in
  rev (aux [] [] xs);;

(* 

rlwrap ocaml

#use "./09_pack.ml";;

pack ["a"; "b"; "b"; "a"];;

pack ["a"; "b"; "c"; "d"];;

pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;

*)

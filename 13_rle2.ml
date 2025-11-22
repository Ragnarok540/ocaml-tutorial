type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode (xs: 'a list): 'a rle list =
  let cons (count: int) (x: 'a): 'a rle =
    if count = 0 then
      One x
    else
      Many (count + 1, x)
  in
  let rec encode_aux count acc = function
    | [] -> []
    | [x] -> cons count x :: acc
    | a :: (b :: _ as t) ->
      if a = b then
        encode_aux (count + 1) acc t
      else
        encode_aux 0 (cons count a :: acc) t
  in List.rev (encode_aux 0 [] xs);;

(* 

rlwrap ocaml

#use "./13_rle2.ml";;

encode ["a"; "b"; "b"; "a"];;

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;

*)

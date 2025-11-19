#use "./09_pack.ml";;

type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode (xs: 'a list): 'a rle list =
  let rec encode_aux (ls: 'a list list): 'a rle list =
    match ls with
    | [] -> []
    | h :: t ->
      if List.length h > 1 then
        Many (List.length h, List.hd h) :: (encode_aux t)
      else
        One (List.hd h) :: (encode_aux t)
  in encode_aux (pack xs);;

(* 

rlwrap ocaml

#use "./11_rle.ml";;

encode ["a"; "b"; "b"; "a"];;

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;

*)

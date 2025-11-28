#use "./04_length.ml";;
#use "./09_pack.ml";;

let encode (xs: 'a list): (int * 'a) list =
  let rec encode_aux (ls: 'a list list): (int * 'a) list =
    match ls with
    | [] -> []
    | h :: t -> (length h, List.nth h 0) :: (encode_aux t)
  in encode_aux (pack xs);;

let encode_short list =
    List.map (fun l -> (List.length l, List.hd l)) (pack list);;

let encode2 list =
    List.map (fun l -> (List.hd l, List.length l)) (pack list);;

(* 

rlwrap ocaml

#use "./10_encode.ml";;

encode ["a"; "b"; "b"; "a"];;

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;

encode2 ["a"; "b"; "b"; "a"];;

*)

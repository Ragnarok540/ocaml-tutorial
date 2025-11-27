let rec push_elem (l: 'a) (xs: 'a list list): 'a list list =
  match xs with
  | [] -> []
  | h :: t ->
    (l :: h) :: push_elem l t;;

let rec individual (xs: 'a list): 'a list list =
  match xs with
  | [] -> []
  | h :: t ->
    [h] :: individual t;;

let rec extract (n: int) (xs: 'a list): 'a list list =
  let len = List.length xs in
    match xs with
    | [] -> []
    | h :: t ->
      if n > len then
        [[]]
      else if n = len then
        [xs]
      else if n = 1 then
        individual xs
      else
        push_elem h (extract (n - 1) t) @ (extract n t);;

(* 

rlwrap ocaml

#use "./26_extract.ml";;

push_elem 1 [ [ 2; 3 ]; [ 2; 4 ]; [ 3; 4 ] ];;

individual ["a"; "b"; "c"; "d"];;

extract 2 ["a"; "b"; "c"; "d"];;

List.length (extract 3 [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12]);;

*)

let rec rev (xs: 'a list): 'a list =
  match xs with
  | [] -> []
  | x :: tail -> (rev tail) @ [x];;

let rec revint (xs, acc: int list * int list): int list =
  match xs with
  | [] -> []
  | x :: tail -> revint (tail, (x :: acc))

(* 

rlwrap ocaml

#use "./05_rev.ml";;

rev ["a"; "b"; "c"; "d"];;

#trace revint;;

revint ([1; 2; 3; 4], []);;

*)

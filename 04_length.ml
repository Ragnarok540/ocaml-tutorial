let rec length (xs: 'a list): int =
  match xs with
  | _ :: tail -> 1 + length tail
  | [] -> 0;;

let length_tail (xs: 'a list): int =
  let rec length_tail' (xs: 'a list) (result: int): int =
    match xs with
    | [] -> result
    | _ :: tail -> length_tail' tail (result + 1)
  in length_tail' xs 0;;

let best_length (xs: 'a list): int =
  let result = ref 0 in
  let ys = ref xs in
  while !ys != [] do
    result := !result + 1;
    ys := List.tl !ys
  done;
  !result

(* 

rlwrap ocaml

#use "./04_length.ml";;

length ["a"; "b"; "c"; "d"];;

length_tail ["a"; "b"; "c"; "d"];;

best_length ["a"; "b"; "c"; "d"];;

*)
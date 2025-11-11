let rec at (n: int) (xs: 'a list): 'a option =
  match xs with
  | x :: _ when n == 0 -> Some x
  | _ :: tail when n > 0 -> at (n - 1) tail
  | _ -> None;;

(* 

rlwrap ocaml

#use "./03_at.ml";;

at 1 ["a"; "b"; "c"; "d"];;

*)

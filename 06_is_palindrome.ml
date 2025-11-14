#use "./05_rev.ml";;

let is_palindrome (xs: 'a list): bool =
  xs = rev xs;;

(* 

rlwrap ocaml

#use "./06_is_palindrome.ml";;

is_palindrome ["a"; "b"; "b"; "a"];;

is_palindrome ["a"; "b"; "c"; "d"];;

*)

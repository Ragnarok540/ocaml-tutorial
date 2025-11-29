#use "./30_gcd.ml";;

let coprime (n: int) (m: int): bool =
  gcd n m = 1;;

(* 

rlwrap ocaml

#use "./31_coprime.ml";;

coprime 13 27;;

coprime 20536 7826;;

*)

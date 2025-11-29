let is_prime (n: int): bool =
  let n = abs n in
  let rec is_not_divisor (d: int): bool =
    d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
  in n > 1 && is_not_divisor 2;;

(* 

rlwrap ocaml

#use "./29_is_prime.ml";;

is_prime 1;;

is_prime 2;;

is_prime 7;;

is_prime 100;;

*)

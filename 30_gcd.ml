let rec gcd (n: int) (m: int): int =
  if m = 0 then n
  else gcd m (n mod m);;

(* 

rlwrap ocaml

#use "./30_gcd.ml";;

gcd 13 27;;

gcd 20536 7826;;

*)

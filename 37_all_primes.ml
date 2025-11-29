#use "./22_range.ml";;
#use "./29_is_prime.ml";;

let all_primes (i: int) (j: int): int list =
  let rng = range i j in
  let rec all_primes_aux (xs: int list): int list =
    match xs with
    | [] -> []
    | h :: t ->
      if is_prime h then
        h :: all_primes_aux t
      else
        all_primes_aux t
  in all_primes_aux rng;;

(* 

rlwrap ocaml

#use "./37_all_primes.ml";;

all_primes 2 100;;

List.length (all_primes 2 7920);;

*)

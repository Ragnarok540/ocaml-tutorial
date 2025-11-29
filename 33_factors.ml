#use "./37_all_primes.ml";;

let factors (n: int): int list =
  let primes = all_primes 2 n in
  let rec factors_aux (pr: int list) (n: int): int list =
    match pr with
    | [] -> [] 
    | h :: t ->
      if n mod h = 0 then
        h :: factors_aux pr (n / h)
      else factors_aux t n
  in factors_aux primes n;;

(* 

rlwrap ocaml

#use "./33_factors.ml";;

factors 315;;

factors 864;;

*)

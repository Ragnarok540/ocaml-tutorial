#use "./37_all_primes.ml";;

let combine2 (a: 'a list) (b: 'a list): ('a * 'a) list =
  List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) b) a);;

let goldbach (n: int): int * int =
  let primes = all_primes 2 n in
  let grid = combine2 primes primes in
  let rec goldbach_aux (gr: (int * int) list) (n: int): (int * int) =
    match gr with
    | [] -> (1, 1)
    | h :: t ->
      let a, b = h in
      if a + b = n then
        h
      else goldbach_aux t n
  in goldbach_aux grid n;;

(* 

rlwrap ocaml

#use "./38_goldbach.ml";;

combine2 (all_primes 2 28) (all_primes 2 28);;

combine2 [true; false] [true; false];;

goldbach 28;;

*)

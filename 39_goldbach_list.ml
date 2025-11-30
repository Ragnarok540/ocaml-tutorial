#use "./22_range.ml";;
#use "./38_goldbach.ml";;

let range_even (i: int) (j: int): int list =
  List.filter (fun x -> x mod 2 = 0) (range i j);;

let goldbach_sum (n: int): (int * (int * int)) list =
  let primes = all_primes 2 n in
  let grid = combine2 primes primes in
  let rec goldbach_sum_aux (gr: (int * int) list) (n: int): (int * (int * int)) list =
    match gr with
    | [] -> []
    | h :: t ->
      let (a, b) = h in
      (a + b, h) :: goldbach_sum_aux t n
  in goldbach_sum_aux grid n;;

let goldbach_list (i: int) (j: int): (int * (int * int)) list =
  let rng = range_even i j in
  let gsum = goldbach_sum j in
  let rec goldbach_list_aux (gs: (int * (int * int)) list) (rn: int list): (int * (int * int)) list =
    match rn with
    | [] -> []
    | h :: t ->
      let e = List.assoc h gs in
      (h, e) :: goldbach_list_aux gs t
  in goldbach_list_aux gsum rng;;

(* 

rlwrap ocaml

#use "./39_goldbach_list.ml";;

range_even 9 20;;

goldbach_sum 20;;

goldbach_list 9 20;;

*)

#use "./31_coprime.ml";;

let phi (m: int): int =
  let rec phi_aux (m: int) (r: int): int =
    if r = m then 0
    else if coprime m r then
      1 + phi_aux m (r + 1)
    else
      phi_aux m (r + 1)
  in phi_aux m 1;;

(* 

rlwrap ocaml

#use "./32_phi.ml";;

phi 10;;

phi 17;;

*)

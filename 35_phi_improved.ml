#use "./34_factors2.ml";;

let rec pow (n: int) (m: int): int =
  if m = 0 then 1
  else n * pow n (m - 1);;

let phi_improved (m: int): int =
  let fact = factors2 m in
  let rec phi_improved_aux (xs: (int * int) list): int =
    match xs with
    | [] -> 1
    | h :: t ->
      let (p, m) = h in
      (p - 1) * pow p (m - 1) * phi_improved_aux t
  in phi_improved_aux fact;;

(* 

rlwrap ocaml

#use "./35_phi_improved.ml";;

pow 2 3;;

phi_improved 10;;

phi_improved 13;;

phi_improved 17;;

*)

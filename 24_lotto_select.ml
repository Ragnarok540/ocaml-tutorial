#use "./20_remove_at.ml";;
#use "./22_range.ml";;

let remove_rand (xs: 'a list): 'a list * 'a =
  let rand_index = Random.int (List.length xs) in
  let l = List.nth xs rand_index in
  (remove_at rand_index xs, l);;

let lotto_select (n: int) (m: int): int list =
  let set = range 1 m in
  let rec lotto_aux (xs: int list) (n: int): int list =
    if n = 0 then
      []
    else
      let ls, i = remove_rand xs in
      i :: lotto_aux ls (n - 1)
  in lotto_aux set n;;

(* 

rlwrap ocaml

#use "./24_lotto_select.ml";;

remove_rand ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"];;

lotto_select 6 49;;

*)

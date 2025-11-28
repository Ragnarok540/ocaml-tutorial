#use "./10_encode.ml";;

let length_sort (xs: 'a list list): 'a list list =
  let length_cmp (i: 'a list) (j: 'a list): int =
    let len_i = List.length i in
    let len_j = List.length j in
    if len_i = len_j then 0
    else if len_i > len_j then 1
    else ~-1
  in List.sort length_cmp xs;;

let frequency (xs: 'a list list): (int * int) list =
  let rec frequency_aux (xs: 'a list list): int list =
    match xs with
    | [] -> []
    | h :: t ->
      List.length h :: frequency_aux t
  in encode2 (List.sort compare (frequency_aux xs));;

let frequency_sort (xs: 'a list list): 'a list list =
  let freqs = frequency xs in
  let frequency_cmp (i: 'a list) (j: 'a list): int =
    let freq_i = List.assoc (List.length i) freqs in
    let freq_j = List.assoc (List.length j) freqs in
    if freq_i = freq_j then 0
    else if freq_i > freq_j then 1
    else ~-1
  in List.sort frequency_cmp xs;;

(* 

rlwrap ocaml

#use "./28_sort.ml";;

length_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"]; ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;

frequency [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"]; ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;

frequency_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"]; ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;

*)

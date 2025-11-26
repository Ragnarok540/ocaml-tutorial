let insert_at (elem: 'a) (n: int) (xs: 'a list): 'a list =
  let rec insert_at_aux (elem: 'a) (n: int) (counter: int) (ls: 'a list) =
    match ls with
    | [] -> []
    | h :: t ->
      if n = counter then
         elem :: h :: t
      else
        h :: insert_at_aux elem n (counter + 1) t
  in insert_at_aux elem n 0 xs;;

(* 

rlwrap ocaml

#use "./21_insert_at.ml";;

insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;

*)

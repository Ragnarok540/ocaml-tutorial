let remove_at (n: int) (xs: 'a list): 'a list =
  let rec remove_at_aux (n: int) (counter: int) (ls: 'a list) =
    match ls with
    | [] -> []
    | h :: t ->
      if n = counter then
        t
      else
        h :: remove_at_aux n (counter + 1) t
  in remove_at_aux n 0 xs;;

(* 

rlwrap ocaml

#use "./20_remove_at.ml";;

remove_at 2 ["a"; "b"; "b"; "a"];;

remove_at 1 ["a"; "b"; "c"; "d"];;

*)

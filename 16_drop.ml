let drop (xs: 'a list) (n: int): 'a list =
  let rec drop_aux (ls: 'a list) (n: int) (counter: int): 'a list =
    match ls with
    | [] -> []
    | h :: t ->
      if counter mod n = 0 then
        drop_aux t n (counter + 1)
      else
        h :: drop_aux t n (counter + 1)
  in drop_aux xs n 1;;

(* 

rlwrap ocaml

#use "./16_drop.ml";;

drop ["a"; "b"; "b"; "a"] 2;;

drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;

*)

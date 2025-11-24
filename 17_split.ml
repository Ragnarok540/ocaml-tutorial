let split (xs: 'a list) (n: int): 'a list * 'a list =
  let rec start (ls: 'a list) (n: int) (counter: int): 'a list =
    match ls with
    | [] -> []
    | h :: t ->
      if counter < n then
        h :: start t n (counter + 1)
      else
        []
  in (start xs n 0, List.rev (start (List.rev xs) (List.length xs - n) 0));;

(* 

rlwrap ocaml

#use "./17_split.ml";;

split ["a"; "b"; "b"; "a"] 2;;

split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;

split ["a"; "b"; "c"; "d"] 5;;

*)

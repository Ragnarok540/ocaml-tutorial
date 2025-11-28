#use "./26_extract.ml";;

let rec groups (xs: 'a list) (sizes: int list): 'a list list list =
  match sizes with
  | [] -> []
  | h :: t ->
    (extract h xs) :: (groups xs t);;

let combine (a: 'a list list) (b: 'a list list): 'a list list list =
  List.concat (List.map (fun e -> List.map (fun e' -> [e; e']) b) a);;

let dup (xs: 'a list list): bool =
  let rec dup_aux (ls: 'a list): bool =
    match ls with
    | [] -> false
    | h :: t ->
      List.exists (fun x -> x = h) t || dup_aux t
  in dup_aux (List.concat xs);; 

let rec clean (xs: 'a list list list): 'a list list list =
  match xs with
  | [] -> []
  | h :: t ->
    if dup h then
      clean t
    else
      h :: clean t;;

let group (xs: 'a list) (sizes: int list): 'a list list list =
  let gs = groups xs sizes in
  let rec group_aux (ls: 'a list list list): 'a list list list =
    match ls with
    | [] -> []
    | h :: [] ->
      combine h []
    | h :: a :: t ->
      combine h a @ group_aux t
  in clean (group_aux gs);;

(* 

rlwrap ocaml

#use "./27_group.ml";;

groups ["a"; "b"; "c"; "d"] [2; 1];;

groups ["a"; "b"; "c"; "d"; "e"] [2; 1; 1];;

combine [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]] [["a"]; ["b"]; ["c"]; ["d"]];;

dup [["a"; "b"]; ["a"]];;

dup [["a"; "b"]; ["c"]];;

clean [[["a"; "b"]; ["a"]]; [["a"; "b"]; ["b"]]; [["a"; "b"]; ["c"]]];;

group ["a"; "b"; "c"; "d"] [2; 1];;

*)

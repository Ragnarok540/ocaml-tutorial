let rec last (xs: 'a list): 'a option =
  match xs with
  | [] -> None
  | [x] -> Some x
  | _ :: tail -> last tail;;

let rec last2 (xs: 'a list): 'a =
  match xs with
  | [] -> failwith "list is empty"
  | [x] -> x
  | _ :: t -> last2 t;;

(*

let l1 = last ["a"; "b"; "c"; "d"];;
print_endline (Option.value l1 ~default:"empty list");;

let l2 = last [];;
print_endline (Option.value l2 ~default:"empty list");;

*)

(* 

rlwrap ocaml

#use "./01_last.ml";;

last ["a"; "b"; "c"; "d"];;

Option.value (last ["a"; "b"; "c"; "d"]);;

last2 ["a"; "b"; "c"; "d"];;

*)

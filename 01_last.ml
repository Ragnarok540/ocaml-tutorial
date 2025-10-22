let rec last (xs: 'a list): 'a option =
  match xs with
  | [] -> None
  | [x] -> Some x
  | _ :: tail -> last tail;;

let l1 = last ["a" ; "b" ; "c" ; "d"];;
print_endline (Option.value l1 ~default:"empty list");;

let l2 = last [];;
print_endline (Option.value l2 ~default:"empty list");;

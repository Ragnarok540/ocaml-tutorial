type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

let rec eval2 (a: string) (val_a: bool) (b: string) (val_b: bool) (expr: bool_expr): bool =
  match expr with
  | Var x ->
    if x = a then val_a
    else if x = b then val_b
    else failwith "invalid variable"
  | Not e -> not (eval2 a val_a b val_b e)
  | And (e1, e2) -> eval2 a val_a b val_b e1 && eval2 a val_a b val_b e2
  | Or (e1, e2) -> eval2 a val_a b val_b e1 || eval2 a val_a b val_b e2;;

let table2 (i: string) (j: string) (expr: bool_expr): (bool * bool * bool) list =
  [(true, true, eval2 i true j true expr);
   (true, false, eval2 i true j false expr);
   (false, true, eval2 i false j true expr);
   (false, false, eval2 i false j false expr)];;


#use "./14_duplicate.ml";;

let rec bool_vars (bits: int): bool list list =
  let rec add_bool (xs: bool list list) (counter: int): bool list list =
    match xs with
    | [] -> []
    | h :: t ->
      if counter mod 2 = 0 then
        (h @ [true]) :: add_bool t (counter + 1)
      else
        (h @ [false]) :: add_bool t (counter + 1) in
  if bits = 1 then
    [[true]; [false]]
  else
    add_bool (duplicate (bool_vars (bits - 1))) 0;;

(* 

rlwrap ocaml

#use "./40_table2.ml";;

eval2 "a" true "b" true (Not (Var "a"));;

eval2 "a" true "b" false (And (Var "a", Var "b"));;

eval2 "a" true "b" false (And (Var "a", Or (Var "a", Var "b")));;

table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")));;

table2 "a" "b" (Not (And (Var "a", Or (Var "a", Var "b"))));;

bool_vars 3;;

*)

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

(* 

rlwrap ocaml

#use "./40_table2.ml";;

eval2 "a" true "b" true (Not (Var "a"));;

eval2 "a" true "b" false (And (Var "a", Var "b"));;

eval2 "a" true "b" false (And (Var "a", Or (Var "a", Var "b")));;

table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")));;

table2 "a" "b" (Not (And (Var "a", Or (Var "a", Var "b"))));;

*)

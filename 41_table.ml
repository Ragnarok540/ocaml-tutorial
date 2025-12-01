#use "./14_duplicate.ml";;

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

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

let rec zip (a: 'a list) (b: 'b list): ('a * 'b) list =
  match (a, b) with
  | ([], []) -> []
  | (h1 :: t1, h2 :: t2) -> (h1, h2) :: zip t1 t2
  | (_, _) -> invalid_arg "lengths don't match";;

let rec eval (xs: (string * bool) list) (expr: bool_expr): bool =
  match expr with
  | Var x -> List.assoc x xs
  | Not e -> not (eval xs e)
  | And (e1, e2) -> eval xs e1 && eval xs e2
  | Or (e1, e2) -> eval xs e1 || eval xs e2;;

let table (vars: string list) (expr: bool_expr): ((string * bool) list * bool) list =
  let b_vars = List.map (zip vars) (bool_vars (List.length vars)) in
  let rec table_aux (vars: (string * bool) list list) (expr: bool_expr): ((string * bool) list * bool) list =
    match vars with
    | [] -> []
    | h :: t ->
      (h, eval h expr) :: table_aux t expr
  in table_aux b_vars expr;;

(* 

rlwrap ocaml

#use "./41_table.ml";;

bool_vars 3;;

zip ["a"; "b"] [true; false];;

List.map (zip ["a"; "b"]) (bool_vars 2);;

eval [("a", true); ("b", false)] (Var "a");;

eval [("a", true); ("b", false)] (Not (Var "a"));;

eval [("a", true); ("b", false)] (And (Var "a", Var "b"));;

eval [("a", true); ("b", false)] (And (Var "a", Or (Var "a", Var "b")));;

table ["a"; "b"] (And (Var "a", Or (Var "a", Var "b")));;

table ["a"; "b"; "c"] (Not (And (Var "a", Or (Var "a", Var "c"))));;

*)

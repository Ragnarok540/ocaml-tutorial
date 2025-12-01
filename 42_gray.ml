#use "./14_duplicate.ml";;

let rec binary (bits: int): string list =
  let rec add_bit (xs: string list) (counter: int): string list =
    match xs with
    | [] -> []
    | h :: t ->
      if counter mod 2 = 0 then
        (h ^ "0") :: add_bit t (counter + 1)
      else
        (h ^ "1") :: add_bit t (counter + 1) in
  if bits = 1 then
    ["0"; "1"]
  else
    add_bit (duplicate (binary (bits - 1))) 0;;

let explode (s: string): char list =
  List.init (String.length s) (String.get s);;

let implode (chars: char list): string = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf;;

let bin_to_gray (b: string): string =
  let bin = explode b in
  let rec bin_to_gray_aux (b: char list): char list =
    match b with
    | [] -> []
    | [x] -> []
    | i :: (j :: _ as t) ->
      if i = j then
        '0' :: bin_to_gray_aux t
      else
        '1' :: bin_to_gray_aux t
  in implode (b.[0] :: (bin_to_gray_aux bin));;

let gray (bits: int): string list =
  List.map bin_to_gray (binary bits);;

(* 

rlwrap ocaml

#use "./42_gray.ml";;

binary 2;;

explode "abc";;

implode ['a'; 'b'; 'c'];;

bin_to_gray "100";;

gray 1;;

gray 2;;

gray 3;;

*)

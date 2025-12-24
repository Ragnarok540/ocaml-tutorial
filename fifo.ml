module Heap =
  struct
    type 'a queue = { data: 'a array; mutable last_index: int }
    let parent (index: int): int = (index - 1) / 2
    let left_child (index: int): int = 2 * index + 1
    let right_child (index: int): int = 2 * index + 2
    let make size = { data = Array.make size 0; last_index = -1 }
    let put position element q = q.data.(position) <- element
    let get position q = q.data.(position)
    let swap i j q =
      let i_elem = q.data.(i) in
      let j_elem = q.data.(j) in
      put i j_elem q;
      put j i_elem q;
      q
    let rec heapify_up index q =
      if index = 0 || (q.data.(parent index) > q.data.(index)) then
        q
      else
        heapify_up (parent index) (swap (parent index) index q)
    let insert elem q =
      q.last_index <- q.last_index + 1;
      put q.last_index elem q;
      heapify_up q.last_index q
    let rec heapify_down index q =
      let current_val = q.data.(index) in
      let left_index = q.data.(left_child index) in
      let left_val = q.data.(left_index) in
      let right_index = q.data.(right_child index) in
      let right_val = q.data.(right_index) in
      if left_index > (q.last_index + 1) || right_index > (q.last_index + 1) then
        q
      else if current_val > left_val && current_val > right_val then
        q
      else if right_val > left_val then
        heapify_down right_index (swap index right_index q) 
      else heapify_down left_index (swap index left_index q)
    let extract_max q =
      let last_e = q.data.(q.last_index) in
      put 0 last_e q;
      put q.last_index 0 q;
      q.last_index <- q.last_index - 1;
      heapify_down 0 q
    end;;

(*
rlwrap ocaml
#use "./fifo.ml";;
*)

open Heap;;
let ff = make 128;;
insert 2 ff;;
insert 1 ff;;
insert 3 ff;;
insert 8 ff;;
insert 2 ff;;

(* #trace extract_max;; *)

extract_max ff;;
(* extract_max ff;; *)
(* extract_max ff;; *)
(* extract_max ff;; *)
(* extract_max ff;; *)


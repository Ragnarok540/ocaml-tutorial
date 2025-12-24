module Heap =
  struct
    type 'a queue = { data: 'a array } (* ; mutable last_index: int *)
    let parent (index: int): int = (index - 1) / 2
    let left_child (index: int): int = 2 * index + 1
    let right_child (index: int): int = 2 * index + 2
    let make = { data = Array.make 128 0 }
    let put position element q =
      q.data.(position) <- element
    let swap i j q =
      let i_elem = q.data.(i) in
      let j_elem = q.data.(j) in
      q.data.(i) <- j_elem;
      q.data.(j) <- i_elem;
      q
    let rec heapify_up index q =
      if index = 0 || (q.data.(parent index) > q.data.(index)) then
        q
      else
        heapify_up (parent index) (swap (parent index) index q)
    exception Full
    let insert elem q =
      let index = Array.find_index (fun x -> x == 0) q.data in
      match index with
      | Some v ->
        put v elem q;
        heapify_up v q
      | None -> raise Full
  end;;


(* 
#use "./fifo.ml";;
open Heap;;
let ff = make;;
insert 2 ff;;
insert 1 ff;;
insert 3 ff;;
*)

#use "./01_last.ml";;
#use "./18_slice.ml";;

let parent (index: int): int =
  (index - 1) / 2;;

let left_child (index: int): int =
  2 * index + 1;;

let right_child (index: int): int =
  2 * index + 2;;

let swap (heap: int list) (i: int) (j: int): int list =
  let len = List.length heap in
  if i >= len || j >= len then failwith "index outside bounds" else
  let i_elem = List.nth heap i in
  let j_elem = List.nth heap j in
  let rec swap_aux (heap: int list) (counter: int): int list =
    match heap with
    | [] -> []
    | h :: t ->
      if counter = i then
        j_elem :: swap_aux t (counter + 1)
      else if counter = j then
        i_elem :: swap_aux t (counter + 1)
      else h :: swap_aux t (counter + 1)
  in swap_aux heap 0;;

let rec heapify_up (heap: int list) (index: int): int list =
  if index = 0 || (List.nth heap (parent index) > List.nth heap index) then
    heap
  else
    heapify_up (swap heap (parent index) index) (parent index);;

let insert (heap: int list) (e: int): int list =
  let new_heap = heap @ [e] in
  heapify_up new_heap ((List.length new_heap) - 1);;

let rec heapify_down (heap: int list) (index: int): int list =
  let len = List.length heap in
  let current_val = List.nth heap index in
  let left_index = left_child index in
  let left_val = List.nth heap left_index in
  let right_index = right_child index in
  let right_val = List.nth heap left_index in
  if left_index > len || right_index > len then
    heap
  else if current_val > left_val && current_val > right_val then
    heap
  else if right_val > left_val then
    heapify_down (swap heap index right_index) right_index
  else heapify_down (swap heap index left_index) left_index;;

let extract_max (heap: int list): int list * int =
  if List.is_empty heap then failwith "heap is empty" else
  let max = List.nth heap 0 in
  let last_e = last2 heap in
  let new_heap = last_e :: slice heap 1 (List.length heap - 2) in
  (heapify_down new_heap 0, max);;

let rec build_heap (array: int list): int list =
  match array with
  | [] -> []
  | h :: t -> insert (build_heap t) h;;

let heap_sort (array: int list): int list =
  let heap = build_heap array in
  let rec heap_sort_aux(array: int list): int list =
    let (heap, max) = extract_max array in
    if List.length heap = 1 then [max] @ heap
    else max :: heap_sort_aux heap
  in heap_sort_aux heap;;

(* 

rlwrap ocaml

#use "./binary_heap.ml";;

let heap = insert (insert (insert (insert [] 2) 1) 3) 8;;

let heap2 = insert heap 2;;

extract_max heap2;;

build_heap [2;1;3;8;2];;

build_heap [45;13;12;16;9;5];;

let heap3 = build_heap [-1;1;5;0;0;4;6;7];;

extract_max heap3;;

let heap3 = build_heap [~-1;1;5;0;0;4;6;7];;

extract_max heap3;;

heap_sort [2;1;3;8;2];;

heap_sort [-1;1;5;0;0;4;6;7];;

heap_sort [45;13;12;16;9;5];;

*)

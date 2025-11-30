#load "unix.cma"
#use "./32_phi.ml";;
#use "./35_phi_improved.ml";;

let timeit (f: 'a -> 'b) (x: 'a): float =
  let t0 = Unix.gettimeofday() in
    ignore (f x);
  let t1 = Unix.gettimeofday() in
    t1 -. t0;;

(* 

rlwrap ocaml

#use "./36_timeit.ml";;

timeit phi 10090;;

timeit phi_improved 10090;;

*)

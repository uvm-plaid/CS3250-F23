(* Problem 2 *)

let a = 10 in let b = 2 * a in a mod b;;

let rec innerzip ls =
   match ls with
      ([], _) -> []
    | (_, []) -> []
    | ((x::xs), (y::ys)) -> (x,y)::(innerzip (xs,ys))
in
innerzip ([2;5], [true;false]);;

(* Problem 3 *)

(*

false + (3 * 0);;

5 + (if 2 > 4 then false else 3);;

if 3 = (1 + 2) then "true" else "not true";;

let rec sum x = 1 + sum x in sum 4;;

(fun y -> not y );;

 *)

(* Problem 4 *)

type 'a plist = Nil | Cons of 'a * 'a plist;;

(*
   filter : ('a -> bool) -> 'a plist -> 'a plist
   in: a predicate p, and a plist l (in curried style)
   out: a plist l' containing all the values v in l such that
        p(v) evaluates to true, with list ordering preserved.
*)
let rec filter p l = Nil;; (* COMPLETE ME *)

let nums = (Cons(2, Cons(3, Cons(5, Cons(8, Nil)))));;

filter (fun x -> (x mod 2) = 0) nums;;

filter (fun x -> (x mod 2) <> 0) nums;;

(* Problem 5 *)

let x = ref 0 in let _ = Cons((x := 1; 1),(x := 2; Nil)) in !x;;

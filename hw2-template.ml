(* CS3250 Programming Languages F23: Homework 2 *)

(* Template, by ceskalka *)

(*
   NOTE: you should complete all parts where it says COMPLETE ME. Curently most are these
   are functions with dummy bodies that are not correct solutions.
*)

(* Problem 1 *)

type nat = Zero | Succ of nat

(* 
   sum : nat -> nat -> nat
   in : nats n1 and n2 
   out : nat that denotes n1 + n2
*)
let rec sum n1 n2 =
  match n1 with
    Zero -> n2
  | (Succ n) -> Succ(sum n n2)

(* 
   mult : nat -> nat -> nat
   in : pair (x, y) where x denotes a number n1 and y denotes n2
   out : z that denotes n1 * n2
 *)

let rec mult n1 n2 = n2 (* COMPLETE ME *)

(* Problem 2 *)

(* 
   Please include your answer here. A few sentences will suffice, but say as much as you 
   like. COMPLETE ME 
 *)

(* Problem 3 *)

(* 
   member : 'a -> 'a list -> bool
   in  : Value v, list l
   out : True iff v is in l
*)
let rec member v l = false (* COMPLETE ME *)

(* Problem 4 *)

(* measurement datatype *)
type emeasures = Meter of float | Liter of float | Centigrade of float
type ameasures = Feet of float | Gallon of float | Fahrenheit of float

(*
  conversion : emeasures -> ameasures
  in : english measurement x
  out : conversion of x into corresponding american
        measurement
*)
let conversion em = (Feet 0.0)  (* COMPLETE ME *)

(* Problem 5 *)

(* tree datatype *)
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

(*
  lookup : ('a * 'a -> bool) -> 'a -> 'a tree -> bool
  in : strict total order lt, element x, tree t possessing 
       BST property
  out : true iff there exists y in t with eq(x,y)
*)
let rec lookup lt x t = false (* COMPLETE ME *)
	
(*
  insert : ('a * 'a -> bool) -> 'a -> 'a tree -> 'a tree
  in : strict total order lt, element x, tree t possessing 
       BST property
  out : tree t' which is t with x inserted such that t' 
        possesses BST property 
*)
let rec insert lt x t = Leaf   (* COMPLETE ME *)


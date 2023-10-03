(* pattern matching *)

let (x,y) = (1,2) in x + y

match (1,2,3) with (0,x,y) -> x * y
                 | (1,x,y) -> x + y
                 | (1,x,y) -> x * y 
                 
let rec fact n =  match n with 0 -> 1 | n -> n * fact(n-1)

let add (x,y) = x+y in add(3,4)

(* polymporphic functions *)

let thrd (_,_,x) = x

let eq x y = x = y

let compose f g = (fun x -> f(g(x)))

let rec double_all l = 
	 match l with
	    [] -> []
	  | x::xs -> (2 * x) :: (double_all xs);;
    
let rec float_all l = 
	 match l with
	    [] -> []
	  | x::xs -> (float x) :: (float_all xs);;
    
let rec map f l = 
	 match l with
	    [] -> []
	  | x::xs -> (f x) :: (map f xs);;
    
map (fun x -> 2 * x) [1;2;3;4];;

map (fun x -> float x) [1;2;3;4];;

let double_all = map (fun x -> 2 * x);;

let float_all = map float;;

let graph = [(1.1,3.7);(6.2,9.4);(5.5,3.8)];;

let xcoords l = map (fun (x,y) -> y) l;;

type student = { name : string; email : string; gpa : float};;

let bob = { name = "bob"; email = "bob@uvm.edu"; gpa = 3.2};;
    
type rt = {a: int; b: bool};;

let rv = { a = 1 + 2; b = not true };;

type ('a,'b) pair = { first: 'a; second: 'b; };;

let p = { first = 5; second = (fun x -> x + 1 ) };;

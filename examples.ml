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

(*
  for_to_do : int -> int -> (int -> unit) -> unit
  in : loop bounds i and j, loop body procedure f
  out : unit
  mutates : whatever f mutates
 *)
let rec for_to_do i j (f : int -> unit) =
  if i >= j then () else (f i; for_to_do (i+1) j f)

(* This iterative version of factorial uses a primitive for loop *)
(* 
  iterfact : int -> int
  in : n
  out : n!
 *)
let iterfact n =     (* x is an accumulator *)
  let x = ref 1 in
  (for i = 0 to (n-1) do
     x := (!x * (n-i))
   done;
   !x)

(* This iterative version of factorial uses a for loop encoded recursively *)
(* 
  iterfact : int -> int      
  in : n
  out : n!
 *)
let iterfact n =
  let x = ref 1 in    (* x is an accumulator *)
  (for_to_do 0 (n-1)
     (fun i -> x := (!x * (n-i)));
   !x)

(* this example exposes order of evaluation of + in OCaml, since the result will be
   1 with right-to-left order of evaluation, and 2 with left-to-right order. *)
let x = ref 0 in (x := 1; 1) + !x;;

exception Foo;;
(* these examples expose order of evaluation of || which uses short-circuiting in Ocaml,   
   since in the first example Foo is raised in a right-to-left order of evaluation while 
   true is the result given left-to-right order, and vice-versa for the second example *)
true || raise Foo;;  
raise Foo || true;;  

(* mutation breaks commutativity of addition, since the result of these two 
   expressions will be different given a fixed order of evaluation of +, showing
   that evaluation of e1 + e2 is not necessarily equal to e2 + e1 in the presence 
   of mutation. *)
let x = ref 0 in (x := 1; !x) + !x;;
let x = ref 0 in !x + (x := 1; !x);;

(* mutation breaks distribution of * over +. The following example shows 
   that evaluation of e1 * (e2 + e3) is not necessarily equivalent to 
   (e1 * e2) + (e1 * e3) in the presence of mutation *)
let x = ref 0 in (x := !x + 1; 1) * ((!x) + (!x));;
let x = ref 0 in ((x := !x + 1; 1)) * (!x) + ((x := !x + 1; 1)) * (!x);;

(* naive fibonacci computation *)
let rec fibnum n = match n with
    0 -> 0
  | 1 -> 1 
  | _ -> fibnum (n-1) + fibnum (n-2);;
 
(* memoized version of fibonacci using a mutable array *)
let rec fibnum_mem n =
  let tab = Array.make (n+1) 0 in 
  let rec fm n = match n with
    0 -> 0
  | 1 -> 1 
  | _ -> if tab.(n) > 0 then tab.(n) else let fn = fm (n-1) + fm (n-2) in (tab.(n) <- fn; fn)
in fm n;;

exception Negative;;

(* total factorial function, raises exception on negative argument *)
let rec fact n = 
  if n < 0 then raise Negative else 
  match n with
     0 -> 1
   | n -> n * fact (n-1);;
   
type 'a option = None | Some of 'a;;

(* this version of factorial handles exception by providing optional result *)
let fact_option n = try Some(fact n) with Negative -> None;;

(* encoding a for loop with recursion *)
let rec for_to_do j k f = 
  	if j > k then () else (f j; for_to_do (j+1) k f)

(* factorial in the procedural style *)
let fact n =
	let soln = ref 1 in 
  	(for_to_do 1 n (fun i -> soln := (!soln * i)); 
   	 !soln)

(*
   Homework 3: Completing the ARITH Interpreter
   
   **You will only need to modify the redx function in this file for the assignment**. 
   See the assignment pdf for details. 

   Completed solutions should be an executable OCaml code file.    
   This assignment template is itself an executable OCaml code file.
 *)

(*
   Abstract Syntax
   ---------------
  
   The expr datatype defines the ASTs for CatML. The mapping from CatML concrete syntax
   to abstract syntax is as follows, in full detail. This mapping is implemented by the
   parser in the online tool at http://ceskalka.w3.uvm.edu/225/catml/catml.php.
 
   [[True]] = Bool(true)
   [[False]] = Bool(false)
   [[n]] = Nat(n)           for any natural number n
   [[e1 + e2]] = Plus([[e1]], [[e2]])
   [[e1 - e2]] = Minus([[e1]], [[e2]])
   [[e1 And e2]] = And([[e1]], [[e2]])
   [[e1 Or e2]] = Or([[e1]], [[e2]])
   [[Not e]] = Not([[e]])
   [[If e1 Then e2 Else e3]] = [[If([[e1]],[[e2]],[[e3]])]]
*)
type expr =
     (* boolean expression forms *)
     Bool of bool | And of expr * expr | Or of expr * expr | Not of expr |
     (* arithmetic expression forms *)
     Nat of int | Plus of expr * expr | Minus of expr * expr | Equal of expr * expr |
     (* conditional expression form *)
     If of expr * expr * expr

(*
  isval : expr -> bool
  in : AST [[e]]
  out : true iff e is a value
*)
let isval e = match e with 
     Nat(_) -> true
   | Bool(_) -> true
   | _ -> false

exception NotReducible

(* 
   redx : expr -> expr
   in : AST [[e]]
   out : AST [[e']] such that e -> e' in the operational semantics, with the stipulation that
   short-circuiting of boolean operations is allowed.
   side effect : exception NotReducible raised if [[e]] isn't reducible in implementation
   (this should be the case if e is a value or is stuck).
   NOTE : This function is incomplete for ARITH.
*)
let rec redx e = match e with
     Not(Bool(b)) -> Bool(not b)
   | And(Bool(b1), Bool(b2)) -> Bool(b1 && b2)
   | Or(Bool(b1), Bool(b2)) -> Bool(b1 || b2)
   | Not(e) -> Not(redx e)
   | And(e1,e2) -> if isval e1 then And(e1, redx e2) else And(redx e1, e2)
   | Or(e1, e2) -> if isval e1 then Or(e1, redx e2) else Or(redx e1, e2)
   | _ -> raise NotReducible   (* as long as the computational and contextual reduction 
                                  rules are defined correctly, non-reducible cases 
                                  can be matched with a wildcard *)

exception StuckExpression

(*
   redxs : expr -> expr
   in : AST [[e]]
   out : [[v]] such that e ->* v in the operational semantics
   side effect : exception StuckExpression raised if [[e]] goes wrong in implementation.
*)
let rec redxs e = match e with 
     Bool(b) -> Bool(b)
   | Nat(n) -> Nat(n)  
   | _ -> redxs (try (redx e) with NotReducible -> raise StuckExpression)

open Printf;;

(*
  prettyPrint : expr -> string
  in : An expression AST [[e]].
  out : The concrete expression e in string format.
*)
let rec prettyPrint e = match e with
   | Bool true -> "True"
   | Bool false -> "False"
   | Nat n -> sprintf "%i" n
   | And (e1, e2) -> "(" ^ (prettyPrint e1) ^ " And " ^ (prettyPrint e2) ^ ")"
   | Or (e1, e2) -> "(" ^ (prettyPrint e1) ^ " Or " ^ (prettyPrint e2) ^ ")"
   | Not e1 -> "(Not " ^ (prettyPrint e1) ^ ")"
   | Plus (e1, e2) -> "(" ^ (prettyPrint e1) ^ " + " ^ (prettyPrint e2) ^ ")"
   | Minus (e1, e2) -> "(" ^ (prettyPrint e1) ^ " - " ^ (prettyPrint e2) ^ ")"
   | Equal (e1, e2) -> "(" ^ (prettyPrint e1) ^ " = " ^ (prettyPrint e2) ^ ")"
   | If(e1, e2, e3) -> "If " ^ (prettyPrint e1) ^ 
                       " Then " ^ (prettyPrint e2) ^
                       " Else " ^ (prettyPrint e3)

(*
  pretty_trace : expr -> bool -> unit
  in : AST [[e]], stepper flag 
  out : ()
  side effects : Pretty prints execution trace of e. Blocks on keystroke 
  between reductions if stepper is on.
*)
let rec pretty_trace e stepper =
  (printf "%s" (prettyPrint e); if stepper then ignore (read_line()) else ();
   if (isval e) then (printf "\n"; flush stdout) else
     try
       (
         let e' = redx e in
         (printf "->\n"; flush stdout; pretty_trace e' stepper)
       )
     with NotReducible ->  (printf "  (Bad, Stuck Expression)\n"; flush stdout))

(*
  stepper : expr -> ()
  in : AST [[e]]
  out : ()
  side effects : Pretty prints execution trace of e. Blocks on keystroke 
  between reductions.
*)
let rec stepper e = pretty_trace e true;;
(*
  tracer : expr -> ()
  in : AST [[e]]
  out : ()
  side effects : Pretty prints execution trace of e. 
*)
let rec tracer e = pretty_trace e false;;

(* Problem 4 plist definition and template *)

type 'a plist = Nil | Cons of 'a * 'a plist;;

(*
   filter : ('a -> bool) -> 'a plist -> 'a plist
   in: a predicate p, and a plist l (in curried style)
   out: a plist l' containing all the values v in l such that
        p(v) evaluates to true, with list ordering preserved.  
*)
let rec filter p l = Nil (* COMPLETE ME *)


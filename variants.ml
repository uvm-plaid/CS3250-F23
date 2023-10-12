type 'a plist = Nil | Cons of 'a * 'a plist

let rec pmap f pl = match pl with
    Nil -> Nil
  | (Cons (x,xs)) -> Cons (f x, pmap f xs) 

(Cons ('l', Cons ('i', Cons ('s', Cons ('t', Nil))))) 

(Cons (3, Cons (1, Cons (7, Nil)))) 

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree;;

Node(Node(Leaf,1,Leaf), 4, Node(Node(Leaf,2,Leaf),1,Leaf));;

Node(Node(Leaf,'a',Leaf), 'b', Node(Node(Leaf,'d',Leaf),'r',Leaf));;

let bst = Node(Node(Leaf,1,Leaf), 4, Node(Node(Leaf,5,Leaf),7,Node(Leaf,9,Node(Leaf,10,Leaf))));;   

let rec inorder t = match t with
    Leaf -> []
  | Node(tl,x,tr) -> (inorder tl) @ [x] @ (inorder tr);;

let rec treemap f t =
      match t with 
         Leaf -> Leaf
       | Node(tl,x,tr) -> Node(treemap f tl, (f x), treemap f tr);;

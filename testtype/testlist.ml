type list = |Nil |Cons of int * list;;

let rec f l = match l with
|Nil -> 0
|Cons(b,q) -> 1 + f q
in f (Cons(2,Cons(4,Nil)))
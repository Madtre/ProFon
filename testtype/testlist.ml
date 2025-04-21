type list = |Nil |Cons of int * list;;

let rec f l = match l with
|Nil -> 0
|Cons(b,q) -> 1
in f Nil
let rec f l1 l2 = match l1 with
|[] -> (match l2 with 
|[]->[]
|p::q->p::(f l1 q))
|p::q-> p::(f q l2)
in f [1;2;3] [4;5;6]
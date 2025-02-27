let rec f l1 l2 accu = match l1 with
|[] -> (match l2 with 
|[]->accu
|p::q->f l1 q (p::accu))
|p::q-> f q l2 (p::accu)
in f [1;2;3] [4;5;6] []
let rec f l = match l with
|[]-> -1
|p::[]->prInt(p)
|p::q->f q
in f [1;2;3]
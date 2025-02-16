let rec f l = match l with
  | []   -> prInt(0)
  | h::q -> prInt(h); f q
in 1
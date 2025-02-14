let rec f x b = if b then x+1 else f (x+1) true in f 0 false

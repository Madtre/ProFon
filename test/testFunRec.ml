let rec f x b = let y = x + 1 in if b then y else f y true in f 0 false

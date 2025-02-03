let rec f x b = let y = x + 1 in if b then f y false else y in f 0 true

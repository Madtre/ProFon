let f x = if x = 0 then E 0 else E 1 in
try raise (f 4) with |E k -> prInt k
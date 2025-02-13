let k = ref 4 in let f () = k := (!k + 1) in f() ; !k

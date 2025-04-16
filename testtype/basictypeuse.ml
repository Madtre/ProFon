type t = Test of int;;

let f x = match x with | Test y -> y+1 in f (Test 2)

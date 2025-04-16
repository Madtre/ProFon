type t = Test of int;;

let a = Test 2 in match a with | Test b -> b
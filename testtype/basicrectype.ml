type t = Test of int | RecTest of t;;
let a = RecTest(Test 2) in a
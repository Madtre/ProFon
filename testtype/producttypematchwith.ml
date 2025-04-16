type t = V of int | Sum of t * t ;;

let rec f x = match x with
|V y -> y
|Sum(a,b) -> f a + f b
in f (Sum(V 2, V 3))
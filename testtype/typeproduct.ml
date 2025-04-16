type t = V of int | Sum of t * t ;;

let a = Sum(V 2, V 3) in a
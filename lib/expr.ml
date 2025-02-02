(* un type pour des expressions arithm�tiques simples *)
type expr =
    Cst of int
  | Bool of bool
  | Add of expr*expr
  | Mul of expr*expr
  | Min of expr*expr
  | And of expr*expr
  | Or of expr*expr
  | Not of expr
  | IfThenElse of expr*expr*expr


(* fonction d'affichage *)
(* NB : dans votre "vraie fouine", il faudra afficher
   du code Caml, et pas des arbres avec des Add, Mul, etc. *)
let rec affiche_expr e =
  (*généralisation de l'affichage à des n uplets*)
  let rec aff_aux s l =
    print_string s; match l with
    |[]->print_string ")"
    |p::q-> (if s = "" then print_string ",") ; affiche_expr p; aff_aux "" q 
  in
  match e with
  | Cst k -> print_int k
  | Bool b -> print_string (if b then "true" else "false")
  | Add(e1,e2) -> aff_aux "Add(" [e1;e2]
  | Mul(e1,e2) -> aff_aux "Mul(" [e1;e2]
  | Min(e1,e2) -> aff_aux "Min(" [e1;e2]
  | And(e1,e2) -> aff_aux "And(" [e1;e2]
  | Or(e1,e2) -> aff_aux "Or(" [e1;e2]
  | Not e -> aff_aux "Not(" [e]
  | IfThenElse(e1,e2,e3) -> aff_aux "IfThenElse(" [e1;e2;e3]
  
(* les valeurs ; pour l'instant �a ne peut �tre que des entiers *)
type valeur = 
|VI of int
|VB of bool

(*Fonctions permettant de cast des valeurs à un type utilisable*)
exception NotAnInt of valeur
exception NotABool of valeur
exception CastError of string

let cast_int (v : valeur) : int = match v with
  |VI k -> k
  |_ -> raise (NotAnInt v)

let cast_bool (v : valeur) : bool = match v with
  |VB b -> b
  |_ -> raise (NotABool v)

let cast_string (v : valeur) : string = match v with
  |VI k -> string_of_int k
  |VB b -> string_of_bool b

(* fin des valeurs *)

(*fin des casts*)
(* s�mantique op�rationnelle � grands pas *)


(* fonction d'affichage des valeurs *)

let affiche_valeur v = print_string (cast_string v)

(* �valuation d'une expression en une valeur *)
let rec eval (e : expr) : valeur = try begin
  match e with
  | Cst k -> VI k
  | Bool b -> VB b
  | Add(e1,e2) -> VI (cast_int (eval e1) + cast_int (eval e2))
  | Mul(e1,e2) -> VI (cast_int (eval e1) * cast_int (eval e2))
  | Min(e1,e2) -> VI (cast_int (eval e1) - cast_int (eval e2))
  | And(e1,e2) -> VB (cast_bool (eval e1) && cast_bool (eval e2))
  | Or(e1,e2) -> VB (cast_bool (eval e1) || cast_bool (eval e2))
  | Not e -> VB (not (cast_bool (eval e)))
  | IfThenElse(e1,e2,e3) -> if cast_bool (eval e1) then eval e2 else eval e3
  end
  with
  |NotAnInt v -> 
    print_newline() ; print_string "Error: NotAnInt in"; print_newline(); affiche_expr e ; print_newline();
    raise (CastError ((cast_string v) ^ " isn't an int"))

  

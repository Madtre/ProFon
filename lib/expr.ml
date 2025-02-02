
let prInt x = print_int x;print_newline(); x;;


(* un type pour des expressions arithm�tiques simples *)
type expr =
    Cst of int
  | Bool of bool
  | Var of string
  | Add of expr*expr
  | Mul of expr*expr
  | Min of expr*expr
  | And of expr*expr
  | Or of expr*expr
  | Not of expr
  | IfThenElse of expr*expr*expr
  | PrInt of expr
  | LetIn of expr*expr*expr


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
  | Var v -> print_string v
  | Add(e1,e2) -> aff_aux "Add(" [e1;e2]
  | Mul(e1,e2) -> aff_aux "Mul(" [e1;e2]
  | Min(e1,e2) -> aff_aux "Min(" [e1;e2]
  | And(e1,e2) -> aff_aux "And(" [e1;e2]
  | Or(e1,e2) -> aff_aux "Or(" [e1;e2]
  | Not e -> aff_aux "Not(" [e]
  | IfThenElse(e1,e2,e3) -> aff_aux "IfThenElse(" [e1;e2;e3]
  | PrInt e -> aff_aux "PrInt(" [e]
  | LetIn(e1,e2,e3) -> aff_aux "LetIn(" [e1;e2;e3]
  
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
  |VI k -> "- : int = " ^ string_of_int k
  |VB b -> "- : bool = " ^ string_of_bool b

(* fin des valeurs *)

(*fin des casts*)
(* s�mantique op�rationnelle � grands pas *)


(* fonction d'affichage des valeurs *)

let affiche_valeur v = print_string (cast_string v)

exception NotAVariable of string
exception UnboundVariable of string

(* �valuation d'une expression en une valeur *)
let eval (e : expr) : valeur = 
  let ctx : (string, valeur) Hashtbl.t = Hashtbl.create 10 in
  let rec eval_aux (e : expr) : valeur = try begin
    match e with
    | Cst k -> VI k
    | Bool b -> VB b
    | Var va -> begin
      try Hashtbl.find ctx va 
      with
      | Not_found -> 
        print_newline() ; raise (UnboundVariable (va ^ " is unbound"))
      end

    | Add(e1,e2) -> VI (cast_int (eval_aux e1) + cast_int (eval_aux e2))
    | Mul(e1,e2) -> VI (cast_int (eval_aux e1) * cast_int (eval_aux e2))
    | Min(e1,e2) -> VI (cast_int (eval_aux e1) - cast_int (eval_aux e2))
    | And(e1,e2) -> VB (cast_bool (eval_aux e1) && cast_bool (eval_aux e2))
    | Or(e1,e2) -> VB (cast_bool (eval_aux e1) || cast_bool (eval_aux e2))
    | Not e -> VB (not (cast_bool (eval_aux e)))
    | IfThenElse(e1,e2,e3) -> if cast_bool (eval_aux e1) then eval_aux e2 else eval_aux e3
    | PrInt e -> let i = cast_int(eval_aux e) in VI(prInt i)
    | LetIn(e1,e2,e3) -> match e1 with
      | Var v -> Hashtbl.add ctx v (eval_aux e2); eval_aux e3
      | _ ->
        print_newline() ; print_string "Error : NotAVariable in " ; print_newline() ; affiche_expr e ; print_newline();
        raise (NotAVariable (cast_string(eval_aux e1) ^ " isn't a variable"))
  end
    with
    |NotAnInt v -> 
      print_newline() ; print_string "Error: NotAnInt in"; print_newline(); affiche_expr e ; print_newline();
      raise (CastError ((cast_string v) ^ " isn't an int"))
    |NotABool v ->
      print_newline() ; print_string "Error: NotABool in"; print_newline(); affiche_expr e ; print_newline();
      raise (CastError ((cast_string v) ^ " isn't a bool"))
    in eval_aux e

  

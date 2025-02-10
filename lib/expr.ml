
let prInt x = print_int x;print_newline(); x;;

let testmode = false;;

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
  | LetRecIn of expr*expr*expr
  | Fun of expr*expr
  | FunCall of expr*expr



(* fonction d'affichage *)
(* NB : dans votre "vraie fouine", il faudra afficher
   du code Caml, et pas des arbres avec des Add, Mul, etc. *)

let rec string_of_expr e : string = 
  (*permet l'affichage de n uplets*)
  let rec aff_aux s l = s ^ (
    match l with
    |[]->")"
    |p::q-> (if s = "" then "," else "") ^ (string_of_expr p) ^ (aff_aux "" q))
  in
  match e with
  | Cst k -> string_of_int k
  | Bool b -> if b then "true" else "false"
  | Var v -> v
  | Add(e1,e2) -> aff_aux "Add(" [e1;e2]
  | Mul(e1,e2) -> aff_aux "Mul(" [e1;e2]
  | Min(e1,e2) -> aff_aux "Min(" [e1;e2]
  | And(e1,e2) -> aff_aux "And(" [e1;e2]
  | Or(e1,e2) -> aff_aux "Or(" [e1;e2]
  | Not e -> aff_aux "Not(" [e]
  | IfThenElse(e1,e2,e3) -> aff_aux "IfThenElse(" [e1;e2;e3]
  | PrInt e -> aff_aux "PrInt(" [e]
  | LetIn(e1,e2,e3) -> aff_aux "LetIn(" [e1;e2;e3]
  | LetRecIn(e1,e2,e3) -> aff_aux "LetRecIn(" [e1;e2;e3]
  | Fun(e1,e2) -> aff_aux "Fun(" [e1;e2]
  | FunCall(e1,e2) -> aff_aux "FunCall(" [e1;e2]
 
let affiche_expr e = print_string (string_of_expr e)

(* les valeurs ; pour l'instant �a ne peut �tre que des entiers *)

type valeur = 
|VI of int
|VB of bool
|VFun of (valeur->valeur)

(*Fonctions permettant de cast des valeurs à un type utilisable*)
exception NotAnInt of valeur
exception NotABool of valeur
exception NotAFunction of valeur
exception CastError of string

let cast_int (v : valeur) : int = match v with
  |VI k -> k
  |_ -> raise (NotAnInt v)

let cast_bool (v : valeur) : bool = match v with
  |VB b -> b
  |_ -> raise (NotABool v)

let cast_fun (v : valeur) : valeur -> valeur = match v with
  |VFun (f) -> f
|_ -> raise (NotAFunction v)

let cast_string (v : valeur) : string = match v with
  |VI k -> "- : int = " ^ string_of_int k
  |VB b -> "- : bool = " ^ string_of_bool b
  |VFun _ -> "- : function"


  (*Sert à stocker dans le contexte des expressions que l'on veut préévaluer, par exemple au lieu de stocker 1+1 on pourra stocker 2*)
(*let cast_expr (v : valeur) : expr = match v with
  |VI k -> Cst k
  |VB b -> Bool b
  |VFun(_, v, def) -> Fun(v, def)*)


(* fonction d'affichage des valeurs *)
type env = (string, valeur) Hashtbl.t

let affiche_env (ctx : env) : unit = Hashtbl.iter (fun x y -> Printf.printf "%s -> %s\n" x (cast_string y)) ctx;;



let affiche_valeur v = print_string (cast_string v)

let castError (e : expr) (v : string) (wrongtype : string) : valeur = 
  print_newline() ; print_string "Error in"; print_newline(); affiche_expr e ; print_newline();
  raise (CastError (v ^ " isn't a " ^ wrongtype))

exception NotAVariable of string
exception UnboundVariable of string
(*Gère l'erreur où on attendrait un nom de variable mais où une autre expression est donnée*)

(* �valuation d'une expression en une valeur *)
let eval (e : expr) : valeur = 
  (*ctx représente le contexte général d'éxécution, en opposition au contexte de chaque fonction*)
  let basectx : env = Hashtbl.create 20 in
  let rec eval_aux (e : expr) (ctx : env): valeur = 
    if testmode then (affiche_expr e ; print_newline() ;affiche_env ctx );
    try begin
    match e with
    | Cst k -> VI k
    | Bool b -> VB b
    | Var va -> begin
      try let ve = Hashtbl.find ctx va in ve
      with
      | Not_found -> 
        print_newline() ; raise (UnboundVariable (va ^ " is unbound"))
      end
    | Add(e1,e2) -> VI (cast_int (eval_aux e1 ctx) + cast_int (eval_aux e2 ctx))
    | Mul(e1,e2) -> VI (cast_int (eval_aux e1 ctx) * cast_int (eval_aux e2 ctx))
    | Min(e1,e2) -> VI (cast_int (eval_aux e1 ctx) - cast_int (eval_aux e2 ctx))
    | And(e1,e2) -> VB (cast_bool (eval_aux e1 ctx) && cast_bool (eval_aux e2 ctx))
    | Or(e1,e2) -> VB (cast_bool (eval_aux e1 ctx) || cast_bool (eval_aux e2 ctx))
    | Not e -> VB (not (cast_bool (eval_aux e ctx)))
    | IfThenElse(e1,e2,e3) -> if cast_bool (eval_aux e1 ctx) then eval_aux e2 ctx else eval_aux e3 ctx
    | PrInt e -> let i = cast_int(eval_aux e ctx) in VI(prInt i)
    | LetIn(e1,e2,e3) ->
      if e1 = Var "_" 
        then let _ = eval_aux e2 ctx in eval_aux e3 ctx
        else let va = getVarName e1 ctx in 
          Hashtbl.add ctx va (eval_aux e2 ctx) ; let v = eval_aux e3 ctx in Hashtbl.remove ctx va; v


    | LetRecIn(e1,e2,e3) ->
      if e1 = Var "_" 
        then failwith "Error : Only variables are allowed as left-hand side of 'let rec'"
      else let funname = getVarName e1 ctx in 
        (match e2 with
        | Fun(v, e) -> 
          let variable = getVarName v ctx in
          Hashtbl.add ctx funname (VFun(defineFunction funname variable e ctx));
          let res = eval_aux e3 ctx
          in Hashtbl.remove ctx funname; res
        | _ -> failwith "Error : This kind of expression is not allowed as right-hand side of 'let rec'")



    | Fun(e1,e2) -> 
      let va = getVarName e1 ctx in
      let fctx = Hashtbl.copy ctx in
      VFun(defineFunction "" va e2 fctx)

    | FunCall(func, value) -> 
      let f = cast_fun(eval_aux func ctx) in
      let v = eval_aux value ctx in
      f v
  end
    with 
    |NotAnInt v -> castError e (cast_string v) "int"
    |NotABool v -> castError e (cast_string v) "bool"
    |NotAFunction v -> castError e (cast_string v) "function"
    |NotAVariable v -> castError e v "variable"


  and getVarName (variable : expr) (ctx : env) : string = match variable with
  |Var v -> v
  |_ -> raise (NotAVariable (cast_string(eval_aux variable ctx)))

  (*On crée une valeur -> valeur par curryfication*)
  and defineFunction (funname : string) (varname : string) (e : expr) (ctx : env) (v : valeur) : valeur = 
    if funname <> "" then Hashtbl.add ctx funname (VFun(defineFunction funname varname e ctx));
    Hashtbl.add ctx varname v;
    let res = eval_aux e ctx in
    Hashtbl.remove ctx varname;
    if funname <> "" then Hashtbl.remove ctx funname;
    res

  in eval_aux e basectx


  


let prInt x = print_int x;print_newline(); x;;

(*A defaut de mieux pour l'instant, on log les warnings avec un print*)
let warning (mess : string) = print_string "Warning : "; print_string mess; print_newline();;

let testmode = false;;

type motif =
|MVar of string
|MUplet of motif list
|MNil
|MCons of motif * motif

(* un type pour des expressions arithm�tiques simples *)
type expr =
    Cst of int
  | Bool of bool
  | Var of string
  | Unit
  | Add of expr*expr
  | Mul of expr*expr
  | Min of expr*expr
  | And of expr*expr
  | Or of expr*expr
  | Not of expr
  | IfThenElse of expr*expr*expr
  | PrInt of expr
  | LetIn of motif*expr*expr
  | LetRecIn of expr*expr*expr
  | Fun of expr*expr
  | FunCall of expr*expr
  | Ref of expr
  | Access of expr
  | Assign of expr*expr
  | Uplet of expr list
  | For of expr*expr*expr*expr
  | While of expr*expr
  | List of expr list


(* fonction d'affichage *)
(* NB : dans votre "vraie fouine", il faudra afficher
   du code Caml, et pas des arbres avec des Add, Mul, etc. *)

let rec string_of_motif (m:motif) : string = 
  let rec aff_aux (s:string) (l:motif list) = s ^ (
    match l with
    |[]->")"
    |p::q-> (if s = "" then "," else "") ^ (string_of_motif p) ^ (aff_aux "" q))
in match m with
  |MVar v -> v
  |MUplet l -> aff_aux "MUplet(" l
  |MNil -> "::[]"
  |MCons(a,l) -> (aff_aux "" [a]) ^ "::" ^ (aff_aux "" [l]) 

let rec string_of_expr (e:expr) : string = 
  (*permet l'affichage de n uplets*)
  let rec aff_aux (s:string) (l:expr list) = s ^ (
    match l with
    |[]->")"
    |p::q-> (if s = "" then "," else "") ^ (string_of_expr p) ^ (aff_aux "" q))
  in
  let rec aff_aux2 (s: string) (m:motif list) (l:expr list) : string = s ^(
    match m with
    |[]-> aff_aux "" l
    |p::q-> (if s = "" then "," else "") ^ (string_of_motif p) ^ (aff_aux2 "" q l))
  in
  match e with
  | Cst k -> string_of_int k
  | Bool b -> if b then "true" else "false"
  | Var v -> v
  | Unit -> "()"
  | Add(e1,e2) -> aff_aux "Add(" [e1;e2]
  | Mul(e1,e2) -> aff_aux "Mul(" [e1;e2]
  | Min(e1,e2) -> aff_aux "Min(" [e1;e2]
  | And(e1,e2) -> aff_aux "And(" [e1;e2]
  | Or(e1,e2) -> aff_aux "Or(" [e1;e2]
  | Not e -> aff_aux "Not(" [e]
  | IfThenElse(e1,e2,e3) -> aff_aux "IfThenElse(" [e1;e2;e3]
  | PrInt e -> aff_aux "PrInt(" [e]
  | LetIn(e1,e2,e3) -> aff_aux2 "LetIn(" [e1] [e2;e3]
  | LetRecIn(e1,e2,e3) -> aff_aux "LetRecIn(" [e1;e2;e3]
  | Fun(e1,e2) -> aff_aux "Fun("[e1;e2]
  | FunCall(e1,e2) -> aff_aux "FunCall(" [e1;e2]
  | Ref e -> aff_aux "Ref(" [e]
  | Access e -> aff_aux "Access(" [e]
  | Assign(e1,e2) -> aff_aux "Assign(" [e1;e2]
  | Uplet(e) -> aff_aux "Uplet(" e
  | For(e1, e2, e3, e4) -> aff_aux "For(" [e1;e2;e3;e4]
  | While(b, e) -> aff_aux "While(" [b;e]
  | List l -> aff_aux "List(" l

 
let affiche_expr e = print_string (string_of_expr e)

(* les valeurs ; pour l'instant �a ne peut �tre que des entiers *)

type valeur = 
|VUnit
|VI of int
|VB of bool
|VFun of (valeur->valeur)
|VRef of valeur
|VUplet of valeur list
|VList of valeur list

(*Fonctions permettant de cast des valeurs à un type utilisable*)
exception NotAnInt of valeur
exception NotABool of valeur
exception NotAFunction of valeur
exception NotARef of valeur
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

let cast_ref (v : valeur) : valeur = match v with
  |VRef r -> r
  |_ -> raise (NotARef v)


let cast_string (v : valeur) : string = 
  let rec getcontent (v : valeur) : string = (match v with
    |VI k -> string_of_int k
    |VB b -> string_of_bool b
    |VUnit -> "()"
    |VFun _ -> "<fun>"
    |VRef r -> "{contents = " ^ (getcontent r) ^ "}"
    |VUplet l -> let (_, upletv) = (uplethelper l ",") in "(" ^ upletv ^ ")"
    |VList l -> let (_, upletv) = (uplethelper l "::") in "(" ^ upletv ^ ")"
  )
  and uplethelper (l : valeur list) (separator : string) : (string * string) = let (uplett, upletv, _) = (List.fold_left 
  
  (fun accu elem -> let (accu1,accu2,b) = accu in 
    (if b 
      then ("valeur * " ^ accu1, accu2 ^ separator ^ (getcontent elem), true) 
      else ("valeur * " ^ accu1, accu2 ^ (getcontent elem), true)
    )
  )
    ("","", false) l) in (uplett, upletv)
  in
    
  match v with
  |VI k -> "- : int = " ^ string_of_int k
  |VB b -> "- : bool = " ^ string_of_bool b
  |VUnit -> "- : unit"
  |VFun _ -> "- : function"
  |VRef _ -> "- : valeur ref = " ^ (getcontent v)
  |VUplet l -> let (uplett, upletv) = uplethelper l "," in
    "- : " ^ uplett ^ " = (" ^ upletv ^ ")"
  |VList l -> let (_, upletv) = uplethelper l "::" in
    "- : valeur list = " ^ upletv ^ "::[]"

let is_ref (v : valeur) : bool = match v with
  |VRef _ -> true
  |_ -> false


(* fonction d'affichage des valeurs *)
type env = (string, valeur) Hashtbl.t

let affiche_env (ctx : env) : unit = Hashtbl.iter (fun x y -> Printf.printf "%s -> %s\n" x (cast_string y)) ctx;;



let affiche_valeur v = print_string (cast_string v)

let castError (e : expr) (v : string) (wrongtype : string) : valeur = 
  print_newline() ; print_string "Error in"; print_newline(); affiche_expr e ; print_newline();
  raise (CastError (v ^ " isn't a " ^ wrongtype))


exception UnboundVariable of string
let findVarValue (var : string) (ctx : env) : valeur = 
  if var = "()" then VUnit
  else try Hashtbl.find ctx var
  with Not_found -> raise (UnboundVariable var)

exception NotAVariable of string
(*Gère l'erreur où on attendrait un nom de variable mais où une autre expression est donnée*)

(* �valuation d'une expression en une valeur *)
let eval (e : expr) : valeur = 
  (*ctx représente le contexte général d'éxécution, en opposition au contexte de chaque fonction*)
  let globalctx : env = Hashtbl.create 20 in
  let basectx : env = Hashtbl.create 20 in
  let rec eval_aux (ctx : env) (e : expr) : valeur = 
    if testmode 
      then (affiche_expr e ; print_newline(); 
    print_string "global ctx : " ; print_newline() ; affiche_env globalctx ; 
    print_string ("local ctx :") ; print_newline() ; affiche_env ctx);
    try begin
    match e with
    | Cst k -> VI k
    | Bool b -> VB b
    | Unit -> VUnit
    | Var va -> findVarValue va ctx      
    | Add(e1,e2) -> VI (cast_int (eval_aux ctx e1) + cast_int (eval_aux ctx e2))
    | Mul(e1,e2) -> VI (cast_int (eval_aux ctx e1) * cast_int (eval_aux ctx e2))
    | Min(e1,e2) -> VI (cast_int (eval_aux ctx e1) - cast_int (eval_aux ctx e2))
    | And(e1,e2) -> VB (cast_bool (eval_aux ctx e1) && cast_bool (eval_aux ctx e2))
    | Or(e1,e2) -> VB (cast_bool (eval_aux ctx e1) || cast_bool (eval_aux ctx e2))
    | Not e -> VB (not (cast_bool (eval_aux ctx e)))
    | IfThenElse(e1,e2,e3) -> if cast_bool (eval_aux ctx e1) then eval_aux ctx e2 else eval_aux ctx e3
    | PrInt e -> let i = cast_int(eval_aux ctx e) in VI(prInt i)
    
    | LetIn(m,e2,e3) ->
      if m = MVar "_" || m = MVar "()"  
        then let _ = eval_aux ctx e2 in eval_aux ctx e3
        else 
          (absorbMotif ctx m (eval_aux ctx e2);
          let res = eval_aux ctx e3 in
          unabsorbMotif ctx m ; res)

    | LetRecIn(e1,e2,e3) ->
      if e1 = Var "_" || e1 = Var "()"
        then failwith "Error : Only variables are allowed as left-hand side of 'let rec'"
      else let funname = getVarName e1 ctx in 
        (match e2 with
        | Fun(v, e) -> 
          let variable = getVarName v ctx in
          Hashtbl.add ctx funname (VFun(defineFunction funname variable e ctx));
          let res = eval_aux ctx e3
          in Hashtbl.remove ctx funname; res
        | _ -> failwith "Error : This kind of expression is not allowed as right-hand side of 'let rec'")



    | Fun(e1,e2) -> 
      let va = getVarName e1 ctx in
      let fctx = Hashtbl.copy ctx in
      VFun(defineFunction "" va e2 fctx)

    | FunCall(func, value) -> 
      let f = cast_fun(eval_aux ctx func) in
      let v = eval_aux ctx value in
      f v

    | Ref e -> VRef(eval_aux ctx e)

    | Access(e) -> findVarValue (getVarName e globalctx) globalctx

    | Assign(var,nvalue) ->
      let varname = getVarName var globalctx in
      let nv = eval_aux ctx nvalue in
      Hashtbl.replace globalctx varname nv;
      (try 
        let v = findVarValue varname ctx in
        match v with
        |VRef _ -> Hashtbl.replace ctx varname (VRef nv)
        |_ -> ()
      with
      |Not_found -> ());

      VUnit

    | Uplet(elist) -> VUplet(List.map (eval_aux ctx) elist)

    | For(v, emin, emax, e) -> let va = getVarName v ctx in
      let imin = cast_int(eval_aux ctx emin) in
      let imax = cast_int(eval_aux ctx emax) in 
      for i = imin to imax do 
        Hashtbl.add ctx va (VI i);
        let res = eval_aux ctx e in
        Hashtbl.remove ctx va;
        if res <> VUnit then warning ((string_of_expr e) ^ " this expression should have type unit")
      done;
      VUnit

    | While(b, e) -> 
      while cast_bool(eval_aux ctx b) do
        if eval_aux ctx e <> VUnit then warning ((string_of_expr e) ^ " this expression should have type unit")        
      done;      
      VUnit
    | List(l) -> VList(List.map (eval_aux ctx) l)

  end
    with 
    |NotAnInt v -> castError e (cast_string v) "int"
    |NotABool v -> castError e (cast_string v) "bool"
    |NotAFunction v -> castError e (cast_string v) "function"
    |NotAVariable v -> castError e v "variable"
    |NotARef v -> castError e (cast_string v) "reference"

    

  and getVarName (variable : expr) (ctx : env) : string = match variable with
  |Var v -> v
  |_ -> raise (NotAVariable (cast_string(eval_aux ctx variable)))
  
  and absorbMotif (ctx : env) (m : motif) (v : valeur) = match m with
  |MVar va -> (*cas classique let v = e2 in e3*)
  (if (is_ref v)
    then Hashtbl.add globalctx va (cast_ref v)) ;
  Hashtbl.add ctx va v
  |MUplet(l1) -> (match v with (*cas uplet let (v1, v2,...) = (e1,e2,...) = e3*)
      |VUplet(l2)-> (try
        List.iter2 (absorbMotif ctx) l1 l2 ;
      with
      |Invalid_argument _ -> failwith "Type Error (les deux uplets n'ont pas la même taille)")
      |_ -> failwith "Type Error (à définir)")
  |MNil -> if (v <> VList([])) then failwith "Erreur lié à un motif étrange / les listes n'ont pas la même longueur" else ()
  |MCons(m, l) -> (match v with
      |VList(p::q) -> absorbMotif ctx m p ; absorbMotif ctx l (VList q)
      |_ -> failwith "Erreur d'absorbtion plutôt regrettable concernant des listes"
  )


  and unabsorbMotif (ctx : env) (m : motif) : unit = match m with
    |MVar va -> Hashtbl.remove ctx va
    |MUplet(l1) -> 
        List.iter (unabsorbMotif ctx) l1
    |MNil -> ()
    |MCons(m, l) -> unabsorbMotif ctx m ; unabsorbMotif ctx l
    
  (*|_ -> failwith "Invalid left-hand side for a 'let in'"*)
  


  (*On crée une valeur -> valeur par curryfication*)
  and defineFunction (funname : string) (varname : string) (e : expr) (ctx : env) (v : valeur) : valeur = 
    if funname <> "" then Hashtbl.add ctx funname (VFun(defineFunction funname varname e ctx));
    (if (is_ref v)
      then Hashtbl.add globalctx varname (cast_ref v)) ;
    Hashtbl.add ctx varname v;
    let res = eval_aux ctx e in
    Hashtbl.remove ctx varname;
    if funname <> "" then Hashtbl.remove ctx funname;
    res



  in eval_aux basectx e


  

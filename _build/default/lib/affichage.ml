open Expr
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
    let rec aff_aux (s:string) l f = s ^ ( (*typer la fonction en 'a list et 'a -> string semble contrarier caml*)
      match l with
      |[]->""
      |p::q-> (if s = "" then "," else "") ^ (f p) ^ (aff_aux "" q f))
    in
    match e with
    | Cst k -> string_of_int k
    | Bool b -> if b then "true" else "false"
    | Var v -> v
    | Unit -> "()"
    | Add(e1,e2) -> aff_aux "Add(" [e1;e2] string_of_expr ^ ")"
    | Mul(e1,e2) -> aff_aux "Mul(" [e1;e2] string_of_expr ^ ")"
    | Div(e1,e2) -> aff_aux "Div(" [e1;e2] string_of_expr ^ ")"
    | Min(e1,e2) -> aff_aux "Min(" [e1;e2] string_of_expr ^ ")"
    | And(e1,e2) -> aff_aux "And(" [e1;e2] string_of_expr ^ ")"
    | Or(e1,e2) -> aff_aux "Or(" [e1;e2] string_of_expr ^ ")"
    | Not e -> aff_aux "Not(" [e] string_of_expr ^ ")"
    | Equal(e1,e2) -> aff_aux "Equal(" [e1;e2] string_of_expr ^ ")"
    | Leq(e1,e2) -> aff_aux "Leq(" [e1;e2] string_of_expr ^ ")"
    | Lt(e1,e2) -> aff_aux "Lt(" [e1;e2] string_of_expr ^ ")"
    | IfThenElse(e1,e2,e3) -> aff_aux "IfThenElse(" [e1;e2;e3] string_of_expr ^ ")"
    | PrInt e -> aff_aux "PrInt(" [e] string_of_expr ^ ")"
    | LetIn(e1,e2,e3,_) -> (aff_aux "LetIn(" [e1] string_of_motif) ^ (aff_aux "" [e2;e3] string_of_expr) ^ ")"
    | LetRecIn(e1,e2,e3,_) -> aff_aux "LetRecIn(" [e1;e2;e3] string_of_expr ^ ")"
    | Fun(e1,e2) -> aff_aux "Fun("[e1;e2] string_of_expr ^ ")"
    | FunCall(e1,e2) -> aff_aux "FunCall(" [e1;e2] string_of_expr ^ ")"
    | Ref e -> aff_aux "Ref(" [e] string_of_expr ^ ")"
    | Access e -> aff_aux "Access(" [e] string_of_expr ^ ")"
    | Assign(e1,e2) -> aff_aux "Assign(" [e1;e2] string_of_expr ^ ")"
    | Uplet(e) -> aff_aux "Uplet(" e string_of_expr ^ ")"
    | For(e1, e2, e3, e4) -> aff_aux "For(" [e1;e2;e3;e4] string_of_expr ^ ")"
    | While(b, e) -> aff_aux "While(" [b;e] string_of_expr ^ ")"
    | List l -> aff_aux "List(" l string_of_expr ^ ")"
    | MatchWith(e, l) -> "MatchWith(" ^ (string_of_expr e) ^ "," ^ (aff_aux "" l (fun (m, e) -> (string_of_motif m) ^ " -> " ^ (string_of_expr e))) ^ ")"
   
let affiche_expr e = print_string (string_of_expr e)

let code_of_expr (e : expr) = match e with
|_->""


let affiche_code e = print_string (code_of_expr e)

let cast_string (v : valeur) : string = 
  let rec getcontent (v : valeur) : string = (match v with
    |VI k -> string_of_int k
    |VB b -> string_of_bool b
    |VUnit -> "()"
    |VFun _ -> "<fun>"
    |VRef r -> "{contents = " ^ r ^ "}"
    |VUplet l -> let (_, upletv) = (uplethelper l ",") in "(" ^ upletv ^ ")"
    |VList [] -> "[]"
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
  |VList [] -> "- : valeur list = []"
  |VList l -> let (_, upletv) = uplethelper l "::" in
    "- : valeur list = " ^ upletv ^ "::[]"


let affiche_env (ctx : env) : unit = Hashtbl.iter (fun x y -> Printf.printf "%s -> %s\n" x (cast_string y)) ctx;;

let affiche_valeur v = print_string (cast_string v)

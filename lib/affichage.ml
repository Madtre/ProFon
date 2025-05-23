open Expr
open Inference
(* fonction d'affichage *)
(* NB : dans votre "vraie fouine", il faudra afficher
   du code Caml, et pas des arbres avec des Add, Mul, etc. *)

let prInt x = if not !src_mode then (print_int x;print_newline()); x;;


   let rec string_of_motif (m:motif) : string = 
    let rec aff_aux (s:string) (l:motif list) = s ^ (
      match l with
      |[]->")"
      |p::q-> (if s = "" then "," else "") ^ (string_of_motif p) ^ (aff_aux "" q))
  in match m with
    |MVar v -> v
    |MUplet l -> aff_aux "MUplet(" l
    |MNil -> "[]"
    |MCons(a,MNil) -> string_of_motif a ^ "::[]"
    |MCons(a,l) -> string_of_motif a ^ "::" ^ string_of_motif l
    |MCustom(s, a) -> s ^ " " ^ string_of_motif a
  
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
    (*| LetIn(e1,e2,e3,_) -> (aff_aux "LetIn(" [e1] string_of_motif) ^ (aff_aux "" [e2;e3] string_of_expr) ^ ")"*)
    | LetIn(e1,e2,e3,b) -> (aff_aux "LetIn(" [e1] string_of_motif) ^ (aff_aux "" [e2;e3] string_of_expr) ^ "," ^ string_of_bool b ^ ")"
    | LetRecIn(e1,e2,e3,_) -> aff_aux "LetRecIn(" [e1;e2;e3] string_of_expr ^ ")"
    | Fun(e1,e2) -> (aff_aux "LetIn(" [e1] string_of_motif) ^ (aff_aux "" [e2] string_of_expr) ^ ")"
    | FunCall(e1,e2) -> aff_aux "FunCall(" [e1;e2] string_of_expr ^ ")"
    | Ref e -> aff_aux "Ref(" [e] string_of_expr ^ ")"
    | Access e -> aff_aux "Access(" [e] string_of_expr ^ ")"
    | Assign(e1,e2) -> aff_aux "Assign(" [e1;e2] string_of_expr ^ ")"
    | Uplet(e) -> aff_aux "Uplet(" e string_of_expr ^ ")"
    | Nil -> "[]"
    | Cons(e1,e2) -> aff_aux "Cons(" [e1;e2] string_of_expr ^ ")"
    | MatchWith(e, l) -> "MatchWith(" ^ (string_of_expr e) ^ (aff_aux "" l (fun (m, e) -> (string_of_motif m) ^ " -> " ^ (string_of_expr e))) ^ ")"
    | TryWith(e1,m,e2) -> "TryWith(" ^ string_of_expr e1 ^ "," ^string_of_motif m ^ "," ^ string_of_expr e2 ^ ")"
    | Raise(e) -> aff_aux "Raise(" [e] string_of_expr
    | TypeDef(_,e) -> "TypeDef(" ^ "," ^ string_of_expr e ^ ")"
    | TypeUse(s,e) -> "TypeUse(" ^s ^ "," ^ string_of_expr e ^ ")"
    | Exception(s) -> "Exception(" ^ string_of_expr s ^ ")"

let affiche_expr e = print_string (string_of_expr e)

type form = |E of expr |M of motif |C of motif*expr
let rec code_of_motif (m : motif) = 
match m with
|MVar s -> s
|MUplet l -> code_aux (List.map (fun e -> M e) l) (List.init (List.length l) (fun i -> if i = 0 then "" else ",")) true
|MNil -> "[]"
|MCons(a,b) -> "(" ^ (code_of_motif a) ^ "::" ^ (code_of_motif b) ^ ")" 
|MCustom(s, a) -> s ^ " " ^ code_of_motif a

and code_of_expr (e : expr) = 

match e with
| Cst k -> string_of_int k
| Bool b -> if b then "true" else "false"
| Var v ->            (*méthode très sécurisée pour désanonymiser les variables, si j'avais le temps j'aurai fait un truc propre avec une Hashtable*)
  if v.[0] = '$' then "a1102356" ^ (String.sub v 1 (String.length v - 1)) else v

| Unit -> "()"
| Add(e1,e2) -> code_aux [E e1;E e2] ["";"+"] true
| Mul(e1,e2) -> code_aux [E e1;E e2] ["";"*"] true
| Div(e1,e2) -> code_aux [E e1;E e2] ["";"/"] true
| Min(e1,e2) -> code_aux [E e1;E e2] ["";"-"] true
| And(e1,e2) -> code_aux [E e1;E e2] ["";"&&"] true
| Or(e1,e2) -> code_aux [E e1; E e2] ["";"||"] true
| Equal(e1,e2) -> code_aux [E e1; E e2] ["";"="] true
| Leq(e1,e2)-> code_aux [E e1; E e2] ["";"<="] true
| Lt(e1,e2)-> code_aux [E e1; E e2] ["";"<"] true
| Not(e) -> code_aux [E e] ["not "] true
| IfThenElse(e1,e2,e3) -> code_aux [E e1;E e2;E e3] ["if " ; " then " ; " else "] true
| PrInt(e)-> code_aux [E e] ["prInt("] true ^ ")"
| LetIn(e1,e2,e3,_)->code_aux [M e1 ; E e2 ; E e3] ["let " ; " = " ; " in "] true
| LetRecIn(e1,e2,e3,_)->code_aux [E e1 ; E e2 ; E e3] ["let rec " ; " = " ; " in "] true
| Fun(e1,e2) -> code_aux [M e1 ; E e2] ["fun " ; " -> "] true
| FunCall(e1,e2) -> code_aux [E e1 ; E e2] ["" ; " "] true
| Ref(e) -> code_aux [E e] ["ref "] true
| Access(e) -> code_aux [E e] ["!("] false ^ ")"
| Assign(e1,e2)-> code_aux [E e1 ; E e2] ["";" := "] true
| Uplet(l) -> code_aux (List.map (fun e -> E e) l) (List.init (List.length l) (fun i -> if i = 0 then "" else ",")) true
| Nil -> "[]"
| Cons(e1,e2) -> code_aux [E e1; E e2] ["";"::"] true
| MatchWith(e, l) -> "(match " ^ code_of_expr e ^ " with \n"^ code_aux (List.map (fun (a,b) -> (C(a,b))) l) (List.init (List.length l) (fun _ -> "")) false ^ ")"
| TryWith(e1,m,e2) -> "(try " ^ code_of_expr e1 ^ " with |E " ^ code_of_motif m ^ " -> " ^code_of_expr e2 ^ ")"
| Raise(e1) -> "(raise" ^code_of_expr(e1) ^ ")"
| TypeDef(_,_) -> "PAS IMPLEMENTE"
| TypeUse(_) -> "PAS IMPLEMENTE"
| Exception(_) -> "PAS IMPLEMENTE"
and code_aux (e : form list) (s: string list) (parenthesis : bool)= (if parenthesis then "(" else "") ^ (match (e,s) with
|([],[])->""
|(f::q1, p2::q2) -> 
  (
  match f with
  |E(p1)-> p2 ^ (code_of_expr p1) ^ (code_aux q1 q2 false)
  |M(m) -> p2 ^ (code_of_motif m) ^ (code_aux q1 q2 false)
  |C(a,b) -> "|" ^ code_of_motif a ^ "->" ^ code_of_expr b ^ "\n" ^ (code_aux q1 q2 false)
  )

|_-> failwith "Mauvaise utilisation de code_of_expr")
^ (if parenthesis then ")" else "")


let affiche_code e = print_string (code_of_expr e)

let rec cast_string (v : valeur) (refcontent : string->valeur) : string = 
  let rec getcontent (v : valeur) (refcontent : string->valeur) : string = (match v with
    |VI k -> string_of_int k
    |VB b -> string_of_bool b
    |VUnit -> "()"
    |VFun _ -> "<fun>"
    |VException k -> getcontent k refcontent
    |VRef r -> "{contents = " ^ cast_string (refcontent r) refcontent ^ "}"
    |VUplet l -> let (_, upletv) = (uplethelper l ",") in "(" ^ upletv ^ ")"
    |VList [] -> "[]"
    |VList l -> let (_, upletv) = (uplethelper l "::") in "(" ^ upletv ^ ")"
    |VCustom (s, l) -> s ^ " (" ^ getcontent l refcontent ^ ")"
  )
  and uplethelper (l : valeur list) (separator : string) : (string * string) = let (uplett, upletv, _) = (List.fold_left 
  
  (fun accu elem -> let (accu1,accu2,b) = accu in 
    (if b 
      then ("valeur * " ^ accu1, accu2 ^ separator ^ (getcontent elem refcontent), true) 
      else ("valeur * " ^ accu1, accu2 ^ (getcontent elem refcontent), true)
    )
  )
    ("","", false) l) in (uplett, upletv)
  in
    
  match v with
  |VI k -> "- : int = " ^ string_of_int k
  |VB b -> "- : bool = " ^ string_of_bool b
  |VUnit -> "- : unit"
  |VFun _ -> "- : function"
  |VRef _ -> "- : valeur ref = " ^ (getcontent v refcontent)
  |VException k -> "- : exn = " ^ getcontent k refcontent
  |VUplet l -> let (uplett, upletv) = uplethelper l "," in
    "- : " ^ uplett ^ " = (" ^ upletv ^ ")"
  |VList [] -> "- : valeur list = []"
  |VList l -> let (_, upletv) = uplethelper l "::" in
    "- : valeur list = " ^ upletv ^ "::[]"
  |VCustom (s, l) -> 
    "- : customtype = " ^ s ^ " (" ^ getcontent l refcontent ^ ")"

let affiche_env (ctx : env) (refcontent : string->valeur) : unit = Hashtbl.iter (fun x y -> Printf.printf "%s -> %s\n" x (cast_string y refcontent)) ctx;;

let affiche_valeur (v : valeur) (refcontent : string->valeur) = print_string (cast_string v refcontent)

let string_of_types (t : types) : string = 
  let rec aux (t : types) : string =
    match t with
    | TAnon n -> "T" ^ string_of_int n
    | TInt -> "int"
    | TBool -> "bool"
    | TArrow (t1, t2) -> "(" ^ (aux t1) ^ "->" ^ (aux t2) ^ ")" 
    | TList (t2) -> (aux t2) ^ " list"
    | TProduct (p::q) -> (aux p) ^ (List.fold_left (fun accu a -> accu ^ " * " ^ (aux a)) "" q)
    | TProduct ([]) -> failwith "impossible"
    | TRef(t2) -> (aux t2) ^ " ref"
    | TUnit -> "unit"
  in
  aux t 

let print_contraintes (contr : (types * types) list) =
  let rec aux (c : (types * types) list) : unit =
    match c with
    | [] -> ()
    | (t1, t2) :: q ->
      Printf.printf "%s - %s\n" (string_of_types t1) (string_of_types t2);
      aux q
  in
  aux contr

let print_assoc (assoc : (string, types) Hashtbl.t) =
  Hashtbl.iter (fun x y -> Printf.printf "%s : %s\n" x (string_of_types y)) assoc;;
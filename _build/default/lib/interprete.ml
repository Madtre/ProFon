open Expr
open Affichage

let cast_int (v : valeur) : int = match v with
  |VI k -> k
  |_ -> raise (NotAnInt v)

let cast_bool (v : valeur) : bool = match v with
  |VB b -> b
  |_ -> raise (NotABool v)

let cast_fun (v : valeur) : (cont*cont) -> valeur -> valeur = match v with
  |VFun (f) -> f
|_ -> raise (NotAFunction v)


let rec cast_type (tctx : (string, string list) Hashtbl.t) (cctx : (string, supportedtype) Hashtbl.t) (s : string) (k,_ : cont*cont) (v : valeur) : valeur =
  let t = Hashtbl.find cctx s in 
  match t with
  |EmptyType -> if v = VUplet([]) then VCustom(s,VUplet([])) else failwith "erreur avec un constructeur vide"
  |Int -> (match v with
    |VI i -> k (VCustom(s,VI i))
    |_ -> raise (NotAnInt v))
  |Boolean -> (match v with
    |VB b -> k (VCustom(s,VB b))
    |_ -> raise (NotABool v))
  |Unit -> (match v with
    |VUnit -> k (VCustom(s,VUnit))
    |_ -> raise (CastError "Not a unit"))
  |CustomType(name) -> 
    let undertypes = Hashtbl.find tctx name in k (VCustom(s,find_undertype undertypes v))
  |ProductType(comp) -> 
    (match v with
    |VUplet(l) -> if matching_type_uplet tctx comp l then k(VCustom(s, v)) else failwith "le type produit ne convient pas"
    |_-> failwith "la valeur n'est pas un uplet, donc pas compatible avec un type produit")
    

and matching_type_uplet (tctx : (string, string list) Hashtbl.t) (types : supportedtype list) (values : valeur list) : bool = match (types,values) with
|([],[])->true
|([],_)|(_,[])->false
|(p1::q1,p2::q2)-> (matching_type tctx p1 p2) && (matching_type_uplet tctx q1 q2)
and matching_type (tctx : (string, string list) Hashtbl.t) (t : supportedtype) (v : valeur) : bool = match (t,v) with
|(Int,VI _) -> true
|(Boolean, VB _) -> true
|(Unit, VUnit) -> true
|(CustomType(s1), VCustom(s2,_)) -> List.exists (fun s -> s = s2) (Hashtbl.find tctx s1)
|(ProductType(comp), VUplet(l)) -> matching_type_uplet tctx comp l
|_->false
(*mériterai plus d'être une fonction auxilliaire que séparé, mais on le sépare pour la visibilité*)    
and find_undertype (sl : string list) (v : valeur) : valeur =
  match v with
  |VCustom(name, l) ->
    begin
      match sl with
      |[] -> failwith "No constructor found matching this value"
      |p::q -> if p = name then VCustom(p,l) else find_undertype q v 
    end
  |_-> failwith "Wrong constructor"
   
  (*|_ -> failwith ("Type error : type  not implemented yet")
*)
let is_ref (v : valeur) : bool = match v with
  |VRef _ -> true
  |_ -> false


let castError (e : expr) (v : string) (wrongtype : string) : valeur = 
  print_newline() ; print_string "Error in"; print_newline(); affiche_expr e ; print_newline();
  raise (CastError (v ^ " isn't a " ^ wrongtype))


exception UnboundVariable of string
let findVarValue (var : string) (ctx : env) : valeur = 
  if var = "()" then VUnit
  else try Hashtbl.find ctx var
  with Not_found -> raise (UnboundVariable var)


  let rec comparaison (v1 : valeur) (v2 : valeur) : bool = match v1, v2 with
  |VI k1, VI k2 -> k1 < k2
  |VB b1, VB b2 -> b1 < b2
  |VUplet l1, VUplet l2 -> if List.length l1 <> List.length l2 then raise(NotComparable(v1,v2)) 
  else List.fold_left2 (fun _ v1 v2 -> if v1 <> v2 then comparaison v1 v2 else false) false l1 l2
  |VList l1, VList l2 -> (match l1 with (*cf comparaison des types algébriques*)
      |[] -> not (l2 = [])
      |p1::q1-> (match l2 with
              |[] -> false
              |p2::q2-> if p1 <> p2 then comparaison p1 p2 else comparaison (VList q1) (VList q2)
      )
  )
  |_, _ -> raise (NotComparable(v1, v2))


(* �valuation d'une expression en une valeur *)
let eval (e : expr) : valeur = 
  if !Expr.src_mode then (
  affiche_code e; (* on n'affiche plus que le code de e *)
  print_newline());
  (*ctx représente le contexte général d'éxécution, en opposition au contexte de chaque fonction*)
  let globalctx : env = Hashtbl.create 20 in
  let typectx : (string, string list) Hashtbl.t = Hashtbl.create 20 in
  let constrctx : (string, supportedtype) Hashtbl.t = Hashtbl.create 20 in
  let caster : string -> (cont*cont) -> valeur -> valeur = cast_type typectx constrctx in
  let basectx : env = Hashtbl.create 20 in
  let deep = ref false in
  let refnumerotation = ref 0 in
  let rec eval_aux (ctx : env) (e : expr) ((k,kE) : cont*cont): valeur = 
    if !debug_mode 
      then (print_string "current expr : " ; affiche_expr e ; print_newline()); 
    if !debug_mode 
    then
      (print_string "global ctx : " ; print_newline() ; affiche_env globalctx (fun s -> findVarValue s globalctx); 
      print_string "local ctx :" ; print_newline() ; affiche_env ctx (fun s -> findVarValue s globalctx) ; print_newline()) ;
    begin
    match e with
    | Cst c -> k (VI c)
    | Bool b -> k (VB b)
    | Unit -> k (VUnit)
    | Var va -> k (findVarValue va ctx)      

    | Add(e1,e2) -> eval_aux ctx e2 ((fun n2 -> eval_aux ctx e1 ((fun n1 -> k (VI(cast_int n1 + cast_int n2))),kE)),kE)
    | Min(e1,e2) -> eval_aux ctx e2 ((fun n2 -> eval_aux ctx e1 ((fun n1 -> k (VI(cast_int n1 - cast_int n2))),kE)),kE)
    | Mul(e1,e2) -> eval_aux ctx e2 ((fun n2 -> eval_aux ctx e1 ((fun n1 -> k (VI(cast_int n1 * cast_int n2))),kE)),kE)
    | Div(e1,e2) -> eval_aux ctx e2 ((fun n2 -> eval_aux ctx e1 ((fun n1 -> k (VI(cast_int n1 / cast_int n2))),kE)),kE)

    | And(e1,e2) -> eval_aux ctx e1 ((fun v1 -> let b1 = cast_bool v1 in if not b1 then k (VB false) else 
                    eval_aux ctx e2 ((fun v2 -> k v2),kE)),kE)
    | Or(e1,e2) ->  eval_aux ctx e1 ((fun v1 -> let b1 = cast_bool v1 in if b1 then k (VB true) else 
                    eval_aux ctx e2 ((fun v2 -> k v2),kE)),kE)
    | Not e -> eval_aux ctx e ((fun v -> k (VB(not (cast_bool v)))),kE)

    | Equal(e1,e2) -> eval_aux ctx e2 ((fun v2 -> eval_aux ctx e1 ((fun v1 -> k (VB(v1 = v2))),kE)),kE)
    | Lt(e1,e2) -> eval_aux ctx e2 ((fun v2 -> eval_aux ctx e1 ((fun v1 -> k (VB(comparaison v1 v2))),kE)),kE)
    | Leq(e1,e2) -> eval_aux ctx e2 ((fun v2 -> eval_aux ctx e1 ((fun v1 -> k (VB(comparaison v1 v2 || v1 = v2))),kE)),kE)
    | IfThenElse(e1,e2,e3) -> 
      eval_aux ctx e1 ((fun v1 -> if cast_bool v1 then eval_aux ctx e2 (k,kE) else eval_aux ctx e3 (k,kE)),kE)
    
    | PrInt e -> eval_aux ctx e ((fun v -> k (VI (prInt (cast_int v)))),kE)
    
    | LetIn(m,e2,e3, b) ->
      if b && !deep then failwith "let a = b ;; utilisé à l'intérieur d'une expression" 
      else let changed = not !deep && not b in if not !deep && not b then deep := true;
      eval_aux ctx e2 ((fun v2 -> absorbMotif ctx m v2 ; eval_aux ctx e3 ((fun v3 -> if changed then deep := false ; unabsorbMotif ctx m ; k v3),kE)),kE)


    | LetRecIn(e1,e2,e3, b) ->
      if b && !deep then failwith "let a = b ;; utilisé à l'intérieur d'une expression" 
      else let changed = not !deep && not b in if not !deep && not b then deep := true;
      if e1 = Var "_" || e1 = Var "()"
        then failwith "Error : Only variables are allowed as left-hand side of 'let rec'"
      else let funname = getVarName e1 ctx in 
        (match e2 with
        | Fun(v, e) -> 
          Hashtbl.add ctx funname (VFun(defineFunction funname v e ctx)); 
          eval_aux ctx e3 ((fun res -> if changed then deep := false ; Hashtbl.remove ctx funname; k res),kE)
        | _ -> failwith "Error : This kind of expression is not allowed as right-hand side of 'let rec'")

    | Fun(e1,e2) -> 
      let fctx = Hashtbl.copy ctx in
      k (VFun(defineFunction "" e1 e2 fctx))

    | FunCall(func, value) -> 
      eval_aux ctx value ((fun v -> eval_aux ctx func ((fun f -> ((cast_fun f) (k,kE) v)),kE)),kE)

    | Ref e -> 
      eval_aux ctx e ((fun v -> Hashtbl.add globalctx (string_of_int !refnumerotation) v ;
      refnumerotation := !refnumerotation + 1;
      k (VRef(string_of_int (!refnumerotation-1)))),kE)
      

    | Access(e) -> eval_aux ctx e ((fun v -> match v with
      |VRef(s) -> k (findVarValue s globalctx)
      |_ -> failwith "expected to ! a ref"
    ),kE)

    | Assign(ap,nvalue) ->
      eval_aux ctx ap ((fun v -> match v with
      |VRef s -> eval_aux ctx nvalue ((fun v2 -> Hashtbl.replace globalctx s v2 ; k VUnit), kE)
      |_ -> failwith "expected to := a ref"),kE)

    | Uplet(elist) -> 
      let rec eval_uplet (elist : expr list) (vlist : valeur list)= ( match elist with
      |[] -> (*failwith "les uplets vides n'existent pas" -> si maintenant*)
        k (VUplet([]))
      |p::[] -> eval_aux ctx p ((fun v -> k (VUplet((*List.rev*) (v::vlist)))),kE)
      |p::q -> eval_aux ctx p ((fun v -> eval_uplet q (v::vlist)),kE)
      )
    in eval_uplet (List.rev elist) []

    | For(_) -> k VUnit
    | While(_) -> k VUnit

    | List(l) -> 
      let eval_list (exprlist : expr list) : valeur = 
        let rec list_aux (elist : expr list) (vlist : valeur list) : valeur =
        (match elist with
        |[] -> k (VList [])
        |p::[] -> eval_aux ctx p ((fun v -> VList(v::vlist)),kE)
        |p::q -> eval_aux ctx p ((fun v -> list_aux q (v::vlist)),kE)
        )
      in match exprlist with
      (*Ici on gère le cas des listes qui ne sont pas terminés par un ::[] explicite*)
      |[]->k (VList([]))
      |p::q-> if p = List([]) then k (list_aux q []) (*Cas de liste dite explicite dans le readme*)
      
      else eval_aux ctx p ((fun v -> (*cas de liste dite implicite dans le readme*)
        match v with
        |VList(l) -> k(match (list_aux q []) with |VList(l2) -> VList(l2@l) |_->failwith "erreur innatendue")
        |_->failwith "liste impropre"
        ),kE)
      in eval_list (List.rev l)

    | MatchWith(e, l) -> 
      eval_aux ctx e ((fun v ->
        
      let rec match_aux (l : (motif * expr) list) : valeur = match l with
      |[] -> failwith "Match error : no matching case"
      |(m, e)::q -> if canabsorbMotif m v then 
        (absorbMotif ctx m v;
        eval_aux ctx e ((fun value -> unabsorbMotif ctx m ; k value),kE)
        )
        else match_aux q
      in match_aux l),kE)
    
    |TryWith(e1,m,e2) -> eval_aux ctx e1 (k, (
      fun v -> let _ = cast_int v in match m with 
      |MVar _ -> absorbMotif ctx m v ; eval_aux ctx e2 (k,kE)
      |_-> failwith "une exception ne peut prendre qu'un int"
      ))

    |Raise e -> eval_aux ctx e ((fun v -> kE v), (fun _ -> failwith "not implemented yet") (*probablement kE ici*))

    | TypeDef((typename, definitions), e) -> 
    
    let constrlist = List.fold_left (fun accu elem -> match elem with
      |TType(name, def) -> Hashtbl.add constrctx name def ; name::accu) [] definitions in

    Hashtbl.add typectx typename constrlist;
    eval_aux ctx e ((fun v -> k v), kE)
  
    | TypeUse(typename) -> 
      k (VFun(caster typename))

  end
    

  and getVarName (variable : expr) (ctx : env) : string = match variable with
  |Var v -> v
  |_ -> raise (NotAVariable (cast_string(eval_aux ctx variable ((fun x -> x),(fun _ -> failwith "Comportement innatendu"))) (fun s -> findVarValue s globalctx)))
  
  (* Les fonctions absorb motif et unabsorb motif font évoluer l'environnement en assignant aux variables dans les motifs les valeurs adaptées*)
  and absorbMotif (ctx : env) (m : motif) (v : valeur) : unit = match m with
  |MVar va -> (*cas classique let v = e2 in e3*)
  if va <> "_" then
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
      |_ -> failwith "Erreur d'absorbtion plutôt regrettable concernant des listes")
  |MCustom(name,mot) -> 
    (match v with
      |VCustom(name2, l) -> if name = name2 then absorbMotif ctx mot l
      |_ -> failwith "Erreur d'absorbtion plutôt regrettable concernant des types customs"
    )


  and unabsorbMotif (ctx : env) (mo : motif) : unit = match mo with
    |MVar va -> Hashtbl.remove ctx va
    |MUplet(l1) -> 
        List.iter (unabsorbMotif ctx) l1
    |MNil -> ()
    |MCons(m, l) -> unabsorbMotif ctx m ; unabsorbMotif ctx l
    |MCustom(_,m) -> unabsorbMotif ctx m

  (*Utile pour le match with*)
  and canabsorbMotif (m : motif) (v : valeur) : bool = match m with
  |MVar _ -> true
  |MUplet(l1) -> (match v with (*cas uplet let (v1, v2,...) = (e1,e2,...) = e3*)
      |VUplet(l2)-> (try
        List.fold_left2 (fun accu p1 p2 -> accu && (canabsorbMotif p1 p2)) true l1 l2 ;
      with
      |Invalid_argument _ -> false)
      |_ -> false)
  |MNil -> v = VList([])
  |MCons(m, l) -> (match v with
      |VList(p::q) -> canabsorbMotif m p && canabsorbMotif l (VList q)
      |_ -> false)
  |MCustom(name,mo) ->
    (match v with
      |VCustom(name2, l) -> if name = name2 then canabsorbMotif mo l else false
      |_ -> false) 


  (*On crée une valeur -> valeur par curryfication*)
  and defineFunction (funname : string) (variableshape : motif) (e : expr) (ctx : env) ((k,kE) : (cont*cont)) (v : valeur) : valeur = 
    (*Si la fonction a un nom, elle est récursive, on l'ajoute donc à son propre contexte*)
    if funname <> "" then Hashtbl.add ctx funname (VFun(defineFunction funname variableshape e ctx));    
    absorbMotif ctx variableshape v;
    eval_aux ctx e ((fun v -> unabsorbMotif ctx variableshape;
    if funname <> "" then Hashtbl.remove ctx funname; k v),kE)



  in let v = eval_aux basectx e ((fun x -> x),(fun _ -> failwith "Raise outside of a try with")) in 
  if !Expr.debug_mode then ( (* on �value e *)
    affiche_valeur v (fun s -> findVarValue s globalctx);
    print_newline());
  v

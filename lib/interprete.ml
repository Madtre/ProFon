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
      |[] -> true
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
  let basectx : env = Hashtbl.create 20 in
  let deep = ref false in
  let refnumerotation = ref 0 in
  let rec eval_aux (ctx : env) (e : expr) ((k,kE) : cont*cont): valeur = 
    if !debug_mode 
      then (print_string "current expr : " ; affiche_expr e ; print_newline()); 
    if !debug_mode 
    then
      (print_string "global ctx : " ; print_newline() ; affiche_env globalctx ; 
      print_string "local ctx :" ; print_newline() ; affiche_env ctx ; print_newline()) ;
    (*try*)begin
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
      else let changed = not !deep in if changed then deep := true;
      eval_aux ctx e2 ((fun v2 -> absorbMotif ctx m v2 ; eval_aux ctx e3 ((fun v3 -> unabsorbMotif ctx m ; if changed then deep := false ; k v3),kE)),kE)

    (*J'ai pas trop confiance en ça*)
    | LetRecIn(e1,e2,e3, b) ->
      if b && !deep then failwith "let a = b ;; utilisé à l'intérieur d'une expression" 
      else let changed = not !deep in if changed then deep := true;
      if e1 = Var "_" || e1 = Var "()"
        then failwith "Error : Only variables are allowed as left-hand side of 'let rec'"
      else let funname = getVarName e1 ctx in 
        (match e2 with
        | Fun(v, e) -> 
          Hashtbl.add ctx funname (VFun(defineFunction funname v e ctx));
          eval_aux ctx e3 ((fun res -> Hashtbl.remove ctx funname; if changed then deep := false ; k res),kE)
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

    | Assign(var,nvalue) ->
      let varname = getVarName var globalctx in
      eval_aux ctx nvalue ((fun nv ->
      (try 
        let v = findVarValue varname ctx in
        match v with
        |VRef s -> Hashtbl.replace globalctx s (nv)
        |_ -> failwith (varname ^ " isn't a ref ")
      
      with
      |Not_found -> raise (UnboundVariable varname)
      ); k VUnit),kE)

    (*Il faut surement list.rev la elist*)
    | Uplet(elist) -> 
      let rec eval_uplet (elist : expr list) (vlist : valeur list)= ( match elist with
      |[] -> failwith "les uplets vides n'existent pas"
      |p::[] -> eval_aux ctx p ((fun v -> k (VUplet((*List.rev*) (v::vlist)))),kE)
      |p::q -> eval_aux ctx p ((fun v -> eval_uplet q (v::vlist)),kE)
      )
    in eval_uplet (List.rev elist) []

    (* L'implémentation des for et while est laissé de côté pour le moment 
    For(v, emin, emax, e) -> let va = getVarName v ctx in
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
    *)
    | For(_) -> k VUnit
    | While(_) -> k VUnit

    | List(l) -> 
      let rec eval_list (elist : expr list) (vlist : valeur list)= ( match elist with
      |[] -> failwith "les uplets vides n'existent pas"
      |p::[] -> eval_aux ctx p ((fun v -> k (VList(List.rev (v::vlist)))),kE)
      |p::q -> eval_aux ctx p ((fun v -> eval_list q (v::vlist)),kE)
      )
    in eval_list (l) []

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

  end
    (*with 
    |NotAnInt v -> castError e (cast_string v) "int"
    |NotABool v -> castError e (cast_string v) "bool"
    |NotAFunction v -> castError e (cast_string v) "function"
    |NotAVariable v -> castError e v "variable"
    |NotARef v -> castError e (cast_string v) "reference"
*)
    

  and getVarName (variable : expr) (ctx : env) : string = match variable with
  |Var v -> v
  |_ -> raise (NotAVariable (cast_string(eval_aux ctx variable ((fun x -> x),(fun _ -> failwith "Comportement innatendu")))))
  
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
      |_ -> failwith "Erreur d'absorbtion plutôt regrettable concernant des listes"
  )


  and unabsorbMotif (ctx : env) (m : motif) : unit = match m with
    |MVar va -> Hashtbl.remove ctx va
    |MUplet(l1) -> 
        List.iter (unabsorbMotif ctx) l1
    |MNil -> ()
    |MCons(m, l) -> unabsorbMotif ctx m ; unabsorbMotif ctx l

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
    
  (*|_ -> failwith "Invalid left-hand side for a 'let in'"*)

  (*On crée une valeur -> valeur par curryfication*)
  and defineFunction (funname : string) (variableshape : motif) (e : expr) (ctx : env) ((k,kE) : (cont*cont)) (v : valeur) : valeur = 
    if funname <> "" then Hashtbl.add ctx funname (VFun(defineFunction funname variableshape e ctx));
    absorbMotif ctx variableshape v;
    eval_aux ctx e ((fun v -> unabsorbMotif ctx variableshape;
    if funname <> "" then Hashtbl.remove ctx funname; k v),kE)



  in let v = eval_aux basectx e ((fun x -> x),(fun _-> failwith "Raise outside of a try with")) in 
  if !Expr.debug_mode then ( (* on �value e *)
    affiche_valeur v;
    print_newline());
  v

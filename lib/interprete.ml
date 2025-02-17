open Expr
open Affichage

let cast_int (v : valeur) : int = match v with
  |VI k -> k
  |_ -> raise (NotAnInt v)

let cast_bool (v : valeur) : bool = match v with
  |VB b -> b
  |_ -> raise (NotABool v)

let cast_fun (v : valeur) : valeur -> valeur = match v with
  |VFun (f) -> f
|_ -> raise (NotAFunction v)

(*let cast_ref (v : valeur) : valeur = match v with
  |VRef r -> r
  |_ -> raise (NotARef v)
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
  let rec eval_aux (ctx : env) (e : expr) : valeur = 
    if !debug_mode 
      then (print_string "current expr : " ; affiche_expr e ; print_newline()); 
    if !debug_mode 
    then
      (print_string "global ctx : " ; print_newline() ; affiche_env globalctx ; 
      print_string ("local ctx :") ; print_newline() ; affiche_env ctx ; print_newline()) ; 
    try begin
    match e with
    | Cst k -> VI k
    | Bool b -> VB b
    | Unit -> VUnit
    | Var va -> findVarValue va ctx      
    | Add(e1,e2) -> VI (cast_int (eval_aux ctx e1) + cast_int (eval_aux ctx e2))
    | Mul(e1,e2) -> VI (cast_int (eval_aux ctx e1) * cast_int (eval_aux ctx e2))
    | Div(e1,e2) -> VI (cast_int (eval_aux ctx e1) / cast_int (eval_aux ctx e2))
    | Min(e1,e2) -> VI (cast_int (eval_aux ctx e1) - cast_int (eval_aux ctx e2))
    | And(e1,e2) -> VB (cast_bool (eval_aux ctx e1) && cast_bool (eval_aux ctx e2))
    | Or(e1,e2) -> VB (cast_bool (eval_aux ctx e1) || cast_bool (eval_aux ctx e2))
    | Not e -> VB (not (cast_bool (eval_aux ctx e)))
    | Equal(e1,e2) -> VB (eval_aux ctx e1 = eval_aux ctx e2)
    | Leq(e1,e2) -> let v1,v2=eval_aux ctx e1, eval_aux ctx e2 in VB(v1 = v2 || (comparaison v1 v2))
    | Lt(e1,e2) -> let v1,v2=eval_aux ctx e1, eval_aux ctx e2 in VB(comparaison v1 v2)
    | IfThenElse(e1,e2,e3) -> if cast_bool (eval_aux ctx e1) then eval_aux ctx e2 else eval_aux ctx e3
    | PrInt e -> let i = cast_int(eval_aux ctx e) in VI(prInt i)
    
    | LetIn(m,e2,e3, b) ->
      if b && !deep then failwith "let a = b ;; utilisé à l'intérieur d'une expression" 
      else let changed = not !deep in if changed then deep := true;
      if m = MVar "_" || m = MVar "()"  
        then let _ = eval_aux ctx e2 in eval_aux ctx e3
        else 
          (absorbMotif ctx m (eval_aux ctx e2);
          let res = eval_aux ctx e3 in
          unabsorbMotif ctx m ; if changed then deep := false; res)

    | LetRecIn(e1,e2,e3, b) ->
      if b && !deep then failwith "let a = b ;; utilisé à l'intérieur d'une expression" 
      else let changed = not !deep in if changed then deep := true;
      if e1 = Var "_" || e1 = Var "()"
        then failwith "Error : Only variables are allowed as left-hand side of 'let rec'"
      else let funname = getVarName e1 ctx in 
        (match e2 with
        | Fun(v, e) -> 
          Hashtbl.add ctx funname (VFun(defineFunction funname v e ctx));
          let res = eval_aux ctx e3
          in Hashtbl.remove ctx funname; if changed then deep := false ; res
        | _ -> failwith "Error : This kind of expression is not allowed as right-hand side of 'let rec'")



    | Fun(e1,e2) -> 
      let fctx = Hashtbl.copy ctx in
      VFun(defineFunction "" e1 e2 fctx)

    | FunCall(func, value) -> 
      let f = cast_fun(eval_aux ctx func) in
      let v = eval_aux ctx value in
      f v

    | Ref e -> Hashtbl.add globalctx (string_of_int !refnumerotation) (eval_aux ctx e) ;
      refnumerotation := !refnumerotation + 1;
      VRef(string_of_int (!refnumerotation-1))

    | Access(e) -> (match eval_aux ctx e with
      |VRef(s) -> findVarValue s globalctx
      |_ -> failwith "expected to ! a ref"
    )

    | Assign(var,nvalue) ->
      let varname = getVarName var globalctx in
      let nv = eval_aux ctx nvalue in
      (try 
        let v = findVarValue varname ctx in
        match v with
        |VRef s -> Hashtbl.replace globalctx s (nv)
        |_ -> failwith (varname ^ " isn't a ref ")
      
      with
      |Not_found -> raise (UnboundVariable varname)
      );
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

    | MatchWith(e, l) -> 
      let v = eval_aux ctx e in
      let rec match_aux (l : (motif * expr) list) : valeur = match l with
      |[] -> failwith "Match error : no matching case"
      |(m, e)::q -> if canabsorbMotif m v then 
        (absorbMotif ctx m v;
        let res = eval_aux ctx e in
        unabsorbMotif ctx m;
        res)
        else match_aux q
      in match_aux l

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
  and defineFunction (funname : string) (variableshape : motif) (e : expr) (ctx : env) (v : valeur) : valeur = 
    if funname <> "" then Hashtbl.add ctx funname (VFun(defineFunction funname variableshape e ctx));
    absorbMotif ctx variableshape v;
    let res = eval_aux ctx e in
    unabsorbMotif ctx variableshape;
    if funname <> "" then Hashtbl.remove ctx funname;
    res



  in let v = eval_aux basectx e in 
  if !Expr.debug_mode then ( (* on �value e *)
    affiche_valeur v;
    print_newline());
  v

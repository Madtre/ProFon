open Expr

type types =
| TAnon of int
| TInt
| TBool
| TArrow of types * types
| TRef of types
| TUnit
| TList of types
| TProduct of types list

let rec code_of_motif (m : motif) = 
  match m with
  |MVar s -> s
  |MUplet _ -> (*code_aux (List.map (fun e -> M e) l) (List.init (List.length l) (fun i -> if i = 0 then "" else ",")) true*) ""
  |MNil -> "[]"
  |MCons(a,b) -> "(" ^ (code_of_motif a) ^ "::" ^ (code_of_motif b) ^ ")" 
  |MCustom(s, a) -> s ^ " " ^ code_of_motif a

let typenumber = ref 1

let infer (e : expr) : ((string,types) Hashtbl.t * (types * types) list) =
let typeslist = ref [] in 
let varassoc = Hashtbl.create 20 in
let rec recinfertypes (e : expr) (t : types) : unit = 
  match e with
  | Cst _ -> typeslist := (t,TInt) :: !typeslist
  | Bool _ -> typeslist := (t,TBool) :: !typeslist
  | Var v -> typeslist := (Hashtbl.find varassoc v,t) :: !typeslist
  | Unit -> typeslist := (t, TUnit) :: !typeslist 
  | Add (e1, e2) -> typeslist := (t,TInt) :: !typeslist ; recinfertypes e1 TInt; recinfertypes e2 TInt
  | Mul (e1, e2) -> typeslist := (t,TInt) :: !typeslist ; recinfertypes e1 TInt; recinfertypes e2 TInt
  | Div (e1, e2) -> typeslist := (t,TInt) :: !typeslist ; recinfertypes e1 TInt; recinfertypes e2 TInt
  | Min (e1, e2) -> typeslist := (t,TInt) :: !typeslist ; recinfertypes e1 TInt; recinfertypes e2 TInt
  | And (e1, e2) -> typeslist := (t,TBool) :: !typeslist ; recinfertypes e1 TBool; recinfertypes e2 TBool
  | Or (e1, e2) -> typeslist := (t,TBool) :: !typeslist ; recinfertypes e1 TBool; recinfertypes e2 TBool
  | Not e -> typeslist := (t,TBool) :: !typeslist ; recinfertypes e TBool
  | Equal (e1, e2) -> typeslist := (t,TBool) :: !typeslist ; let t2 = TAnon !typenumber in typenumber := !typenumber + 1 ;
                      recinfertypes e1 t2; recinfertypes e2 t2
  | Leq (e1, e2) -> typeslist := (t,TBool) :: !typeslist ; let t2 = TAnon !typenumber in typenumber := !typenumber + 1 ;
                    recinfertypes e1 t2; recinfertypes e2 t2
  | Lt (e1, e2) -> typeslist := (t,TBool) :: !typeslist ; let t2 = TAnon !typenumber in typenumber := !typenumber + 1 ;
                  recinfertypes e1 t2; recinfertypes e2 t2
  | IfThenElse (e1, e2, e3) -> recinfertypes e1 TBool; recinfertypes e2 t; recinfertypes e3 t
  | PrInt e -> typeslist := (t,TInt) :: !typeslist ; recinfertypes e TInt
  | LetIn(m, e1, e2, b) -> let t2 = TAnon !typenumber in typenumber := !typenumber + 1 ; 
                          recinfertypes e1 t2 ;
                          infermotif m t2 ;
                          recinfertypes e2 t ; 
                          if b then uninfermotif m 
                          (*Hashtbl.remove varassoc s*)
  (*| LetRecIn (m, e1, e2, _) -> let _,t2 = (infermotif m) in typesassoc := (code_of_motif m, t2):: !typesassoc ; recinfertypes e1 t2 ; recinfertypes e2 t*)
  | LetRecIn (_) -> failwith "Not implemented" 
  | Fun (m, e) -> let t1 = TAnon !typenumber in typenumber := !typenumber + 1 ; 
                  let t2 = TAnon !typenumber in typenumber := !typenumber + 1 ; 
                  infermotif m t2;
                  typeslist := (t, TArrow (t1, t2)) :: !typeslist ;
                  recinfertypes e t2 ; uninfermotif m
  | FunCall (e1, e2) -> let t2 = TAnon !typenumber in typenumber := !typenumber + 1 ; recinfertypes e1 (TArrow(t2, t));
                        recinfertypes e2 t 
  | Ref e -> let t2 = TAnon !typenumber in typenumber := !typenumber + 1 ; 
             typeslist := (t, TRef(t2)) :: !typeslist;
             recinfertypes e t2
  | Access e -> let t2 = TAnon !typenumber in typenumber := !typenumber + 1 ; 
                typeslist := (t, t2) :: !typeslist;
                recinfertypes e (TRef(t2)) 
  | Assign (e1,e2) -> let t2 = TAnon !typenumber in typenumber := !typenumber + 1 ; 
                      typeslist := (t,TUnit):: !typeslist;
                      recinfertypes e1 (TRef t2) ; recinfertypes e2 t2
  | Uplet l -> let l2 = List.map (fun e -> let t2 = TAnon !typenumber in typenumber := !typenumber + 1 ; 
                                  recinfertypes e t2 ; t2) l
              in typeslist := (t,TProduct(l2)) :: !typeslist 
  | Nil -> let t2 = TAnon !typenumber in typenumber := !typenumber + 1 ; 
           typeslist := (t,TList(t2))::!typeslist
  | Cons(a,b) -> let t2 = TAnon !typenumber in typenumber := !typenumber + 1 ; 
                 typeslist := (t, TList(t2))::!typeslist;
                 recinfertypes a t2 ; recinfertypes b (TList(t2))
  | MatchWith _ -> failwith "Not implemented" 
  | TryWith _ -> failwith "Not implemented" 
  | Raise _ -> failwith "Not implemented" 
  | TypeDef _ -> failwith "Not implemented"
  | TypeUse _ -> failwith "Not implemented"
  | Exception _ -> failwith "Not implemented"

and infermotif (m : motif) (t : types) : unit =
  match m with
  | MVar v -> Hashtbl.add varassoc v t ;
  | MCons(a,b) -> let t2 = TAnon !typenumber in typenumber := !typenumber + 1 ;
                  typeslist := (t, TList(t2))::!typeslist;
                  infermotif a t2 ; infermotif b (TList(t2))
  | MNil -> let t2 = TAnon !typenumber in typenumber := !typenumber + 1 ;
            typeslist := (t, TList(t2))::!typeslist
  | MUplet(l) -> let l2 = List.map (fun e -> let t2 = TAnon !typenumber in typenumber := !typenumber + 1 ; 
                infermotif e t2 ; t2) l
                in typeslist := (t,TProduct(l2)) :: !typeslist 
  |_-> failwith "Not implemented"

and uninfermotif (m : motif) : unit = 
  match m with
  | MVar v -> Hashtbl.remove varassoc v
  | MCons(a,b) -> uninfermotif a ; uninfermotif b
  | MNil -> ()
  | MUplet(l) -> List.iter uninfermotif l
  | _-> failwith "Not implemented"

in Hashtbl.add varassoc "Expression finale" (TAnon(0)) ; recinfertypes e (TAnon 0) ; (varassoc,!typeslist)
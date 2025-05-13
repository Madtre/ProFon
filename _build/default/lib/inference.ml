open Expr

type types =
| TAnon of int
| TInt
| TBool
| TArrow of types * types

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
  | Unit -> failwith "Not implemented" 
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
  | LetIn(m, e1, e2, _) -> let s,t2 = infermotif m in Hashtbl.add varassoc s t2  ; 
                          recinfertypes e1 t2 ; recinfertypes e2 t ; 
                          (*Hashtbl.remove varassoc s*)
  (*| LetRecIn (m, e1, e2, _) -> let _,t2 = (infermotif m) in typesassoc := (code_of_motif m, t2):: !typesassoc ; recinfertypes e1 t2 ; recinfertypes e2 t*)
  | LetRecIn (_) -> failwith "Not implemented" 
  | Fun (m, e) -> let t2 = TAnon !typenumber in typenumber := !typenumber + 1 ; 
                  let x,y = infermotif m in (Hashtbl.add varassoc x y) ; typeslist := (t, TArrow (y, t2)) :: !typeslist ;
                  recinfertypes e t2 ; Hashtbl.remove varassoc x
  | FunCall (e1, e2) -> let t2 = TAnon !typenumber in typenumber := !typenumber + 1 ; recinfertypes e1 (TArrow(t2, t));
                        recinfertypes e2 t 
  | Ref _ -> failwith "Not implemented" 
  | Access _ -> failwith "Not implemented"
  | Assign (_, _) -> failwith "Not implemented" 
  | Uplet _ -> failwith "Not implemented" 
  | Nil -> failwith "Not implemented" 
  | Cons _ -> failwith "Not implemented" 
  | MatchWith _ -> failwith "Not implemented" 
  | TryWith _ -> failwith "Not implemented" 
  | Raise _ -> failwith "Not implemented" 
  | TypeDef _ -> failwith "Not implemented"
  | TypeUse _ -> failwith "Not implemented"
  | Exception _ -> failwith "Not implemented"

and infermotif (m : motif) : string * types =
  match m with
  | MVar v -> let t2 = TAnon !typenumber in typenumber := !typenumber + 1 ; (v,t2)
  |_-> failwith "Not implemented"
  (*| MUplet l -> typeslist := (t,t) :: !typeslist ; List.iter (fun m -> infermotif m t) l
  | MNil -> ()
  | MCons (m1, m2) -> typeslist := (t,t) :: !typeslist ; infermotif m1 t; infermotif m2 t
  | MCustom (s, m) -> typeslist := (t,t) :: !typeslist ; infermotif m t*)

in Hashtbl.add varassoc "Expression finale" (TAnon(0)) ; recinfertypes e (TAnon 0) ; (varassoc,!typeslist)
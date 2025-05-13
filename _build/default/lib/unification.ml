open Inference
open Affichage

exception UnificationError of string

let rec substitute (v : int) (t : types) (l : (types * types) list) : (types * types) list = 
  let rec substitutetype (v : int) (t : types) (p : types) : types = 
    match p with
    | TAnon v' -> if v = v' then t else p
    | TArrow (t1, t2) -> TArrow((substitutetype v t t1),(substitutetype v t t2))
    | _ -> p
  in match l with
  | [] -> []
  | (t1,t2)::q -> 
    let t1' = substitutetype v t t1 in
    let t2' = substitutetype v t t2 in
    (t1',t2')::(substitute v t q)



let rec insubstitution (v : int) (l : (types * types) list) : bool = 
  match l with
  | [] -> false
  | (t1,_)::q -> 
    match t1 with
              | TAnon v' when v' = v -> true
              | _ -> insubstitution v q

let rec invars (v : int) (t : types) : bool = 
  match t with
  | TAnon v' -> v = v'
  | TArrow (t1, t2) -> invars v t1 || invars v t2
  | _ -> false
  
let unif (l : (types * types) list) : (types * types) list = 
  let rempl = ref [] in
  let rec unif_aux (t1,t2 : types * types) (l : (types * types) list) : (types * types) list = 
    if t1 = t2 then l else
    match (t1,t2) with
    |(TAnon v, _) -> if invars v t2 then raise (UnificationError ("Recursive type " ^ string_of_types t1)) else 
      if not (insubstitution v !rempl) then (rempl := (t1, t2)::(substitute v t2 (!rempl)); substitute v t2 l) else l
    |(_, TAnon v) -> if invars v t1 then raise (UnificationError ("Recursive type " ^ string_of_types t2)) else 
      if not (insubstitution v !rempl) then (rempl := (t2, t1)::(substitute v t1 (!rempl)); substitute v t1 l) else l
    |(TArrow (t11, t12), TArrow (t21, t22)) -> 
      (t11, t21)::(t12,t22)::l
    |_ -> raise (UnificationError ("Incompatible type : " ^ (string_of_types t1) ^ " and " ^ (string_of_types t2)))
  and unif_call (l : (types * types) list) : unit = 
    match l with
    | [] -> ()
    | t::q -> unif_call(unif_aux t q)
  in unif_call l; !rempl

let rec findtype (l : (types * types) list) (t : types) : types = 
  match l with
  | [] -> failwith ("Type not found " ^ (string_of_types t))
  | (t1,t2)::q -> if t = t1 then t2 else if t = t2 then t1 else findtype q t
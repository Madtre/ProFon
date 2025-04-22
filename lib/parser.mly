%{
(* --- PARTIE 1, préambule : ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Cst of int | Add of expr*expr | Mul of expr*expr | Min of expr*expr *)

(*utile pour les fonctions à n variables*)
let rec aux (m : expr) (e : expr) : expr = (*affiche_expr m; affiche_expr e; print_newline() ;*) match m with
|Fun(v, t) -> Fun(v, aux t e)
|LetIn (v,Unit,Unit,_) -> Fun(v, e)
|_ -> failwith "comportement innatendu de la grammaire lors du parsing d'une fonction"

let upletaux (e : expr) (u : expr) : expr = match u with
|Uplet(l) -> Uplet(e::l)
|_-> failwith "comportement innatendu de la grammaire lors du parsing d'un uplet"

let listaux (e : expr) (u : expr) : expr = match u with
|List(l) -> List(e::l)
|_-> failwith "comportement innatendu de la grammaire lors du parsing d'une liste"

let mupletlist (e : motif) (u : motif) : motif = match u with
|MUplet(l) -> MUplet(e::l)
|MCons(a,l)->MCons(e,MCons(a,l))
|_-> failwith "comportement innatendu de la grammaire lors du parsing d'un motif d'uplet"

let matchwithconstr ((m,v) : (motif * expr)) (matching : expr)= match matching with
|MatchWith(c,l) -> MatchWith(c, (m,v)::l)
|_-> failwith "comportement innatendu de la grammaire lors du parsing d'un match"

let varanonyme = ref 0

let parsetype (typestring : string) : supportedtype = match typestring with
|"int"->Int
|"bool"->Boolean
|"unit"->Unit
|_->CustomType(typestring)

%}



/* PARTIE 2, on liste les lexèmes (lien avec le fichier lexer.mll) ******* */                                   
%token <int> INT       /* le lexème INT a un attribut entier */
%token <bool> BOOL
%token <string> VAR
%token <string> CONSTRUCT

%token PLUS TIMES MINUS DIV
%token AND OR NOT
%token LPAREN RPAREN
%token IF THEN ELSE
%token PRINT
%token LET IN EQUAL
%token FUN RIGHTARROW FUNCTION
%token REC
%token REF BANG ASSIGN
%token SEPARATOR DOUBLESEPARATOR
%token COMMA
%token QUATROSPUNTOS LBRACKET RBRACKET
%token MATCH WITH CASE
%token LEQ LT GEQ GT DIFF
%token TRY EXCEPT RAISE
%token BEGIN END
%token TYPE OF

%token EOF             /* retour à la ligne */


/* PARTIE 3, on donne les associativités ********************************* */  


(*%nonassoc UNDERPLUS
*)
%left RIGHTARROW
%left CASE
%left EQUAL

%left THEN
%left ELSE

%left LEQ LT GEQ GT DIFF

%left REF

%left IN


%left PLUS MINUS AND OR   
%left MOINSUNAIRE
%left TIMES DIV NOT
%left DOUBLESEPARATOR
%left PRINT
%right SEPARATOR


/* PARTIE 4, le point d'entrée ******************************************* */
		    
%start main             /* "start" signale le point d'entrée du parser: */
                        /* c'est ici le non-terminal "main", qui est défini plus bas */
%type <Expr.expr> main     /* on _doit_ donner le type associé au point d'entrée "main" */


/* PARTIE 5 : la grammaire, enfin ! ************************************** */                                                         
%%

main:                       /* <- le point d'entrée (cf. + haut, "start") */
                                                (*Permet d'empiler les TypeDef + on reverse l'ordre de la liste pour que les types soit lus dans l'ordre où ils sont écrits*)
|TYPE t=types DOUBLESEPARATOR e=expression EOF  { List.fold_left (fun acc b -> TypeDef(b, acc)) e (List.rev t) }
|TYPE t=types DOUBLESEPARATOR EOF  { List.fold_left (fun acc b -> TypeDef(b, acc)) Unit t }
|TYPE t=types EOF  { List.fold_left (fun acc b -> TypeDef(b, acc)) Unit t }
|e=expression EOF { e }  

types:
| t=typedef { t::[] }
| m=types TYPE t=typedef { t::m }
| m=types DOUBLESEPARATOR TYPE t=typedef { t::m }

typedef:
| name=VAR EQUAL l=typelist            { (name, l) }
| name=VAR EQUAL CASE l=typelist            { (name, l) }

typelist:
| c=typecase CASE l=typelist           { c::l }
| c=typecase                           { c::[] }

typecase:
| c = CONSTRUCT {TType(c,EmptyType)}
| c = CONSTRUCT OF t=typeproduct { 
    match t with
    |[]->failwith "erreur impossible"
    |[t]->TType(c,t)
    |l->TType(c,ProductType(l)) }

typeproduct:
| t=VAR                  { (parsetype t)::[] }
| t=VAR TIMES l=typeproduct { parsetype t::l }

exprseq:
|e=expression                                    { e }
|e1=exprseq SEPARATOR e2=exprseq                 { LetIn(MVar "_",e1,e2,false)}


value:
| i=INT                             { Cst i} 
| b=BOOL                            { Bool b }
| v=VAR                             { Var v }
| LPAREN RPAREN                     { Unit }
| LBRACKET RBRACKET                 { List([])}
| c=CONSTRUCT                       { TypeUse(c,Uplet[]) }
(*TODO : JETER LES CONSTRUCTEURS EN PARAMETRE DES FONCTIONS*)



applic:
| a=applic s=sexpr { match a with |TypeUse(c,Uplet([])) -> TypeUse(c,s) |_ -> FunCall(a, s) } (*cas où on réduit le dernier paramètre*)
| e=sexpr        { e } (*cas de base, il nous reste plus que le nom de la première fonction*)


sexpr:
| v = value                  { v }
| LPAREN e=exprseq RPAREN { e }       
| BEGIN e=exprseq END { e }     
| LBRACKET l = listexprbracket               { l }
| BANG e=sexpr                            { Access(e) }


multivariables:
| mot=smotif m = multivariables { Fun(mot, m)}
| mot=smotif {LetIn(mot, Unit, Unit,false)} (*Pour des raisons de typage on utilise cette structure, elle n'a aucun sens en réalité*)

muplets:
| m=motifupletexpr COMMA u = muplets {mupletlist m u} 
| m=motifupletexpr            {MUplet([m])}

motifupletexpr:
|m = smotif QUATROSPUNTOS l=motifupletexpr  {MCons(m,l)}
|m = smotif { m }

smotif :
|v = VAR                            {MVar(v)}
|LBRACKET RBRACKET                  {MNil}
|LPAREN m = motif RPAREN            {m}

motif:
|v = VAR                      {MVar(v)}
|c = CONSTRUCT                 {MCustom(c,MUplet([]))}
|c = CONSTRUCT m=smotif        {MCustom(c,m)}
|m = motifupletexpr COMMA u=muplets   {mupletlist m u}
|m = smotif QUATROSPUNTOS l=motifupletexpr {MCons(m,l)}
|LPAREN m=motif RPAREN   { m }
|LBRACKET RBRACKET {MNil}

uplets:
| a=internalexpression COMMA u = uplets { upletaux a u}
| a=internalexpression                  {Uplet([a])}

sexprlistb: (*j'ajoute ici un cas particulier pour une expression de la forme [2,3] ; cas semblant très spécifique a priori*)
| a=internalexpression {a}
| e=internalexpression COMMA u=uplets    { upletaux e u }

listexpr:
| e = internalexpression QUATROSPUNTOS l = listexpr { listaux e l }
| e = internalexpression                   { List([e]) }

listexprbracket:
| e = sexprlistb SEPARATOR l = listexprbracket { listaux e l }
| e = sexprlistb RBRACKET               { List(e::(List [])::[])}

cases :
| m = motif RIGHTARROW e = exprseq CASE c = cases    { matchwithconstr (m,e) c }
| m = motif RIGHTARROW e = exprseq              { MatchWith(Unit, [(m,e)]) } (*On met Unit ici tant qu'on ne sait pas ce qu'on va match*)

internalexpression:
| a=applic {a}
| e1=internalexpression PLUS e2=internalexpression      { Add(e1,e2) }
| e1=internalexpression TIMES e2=internalexpression     { Mul(e1,e2) }
| e1=internalexpression MINUS e2=internalexpression     { Min(e1,e2) }
| e1=internalexpression DIV e2=internalexpression       { Div(e1,e2) }
| MINUS e=internalexpression             %prec MOINSUNAIRE       { Min(Cst 0, e) } (* le moins unaire *)  

| e1=internalexpression AND e2=internalexpression       { And(e1,e2) }
| e1=internalexpression OR e2=internalexpression        { Or(e1,e2) }
| NOT e=internalexpression                      { Not(e) }

| e1=internalexpression EQUAL e2=internalexpression     { Equal(e1,e2) }
| e1=internalexpression LEQ e2=internalexpression       { Leq(e1,e2) }
| e1=internalexpression LT e2=internalexpression        { Lt(e1,e2) }
| e1=internalexpression GEQ e2=internalexpression       { Not(Lt(e1,e2)) }
| e1=internalexpression GT e2=internalexpression        { Not(Leq(e1,e2)) }
| e1=internalexpression DIFF e2=internalexpression      { Not(Equal(e1,e2)) }

| REF e=internalexpression                              { Ref e }
| PRINT e=internalexpression                    { PrInt e } 

(*| c=CONSTRUCT e=internalexpression                      { TypeUse(c,e) }
*)
expression:
(*| e=sexpr                               { e }*) (*on fait expression -> sexpr via e -> internalexpression -> applic -> sexpr*)


| IF e1=expression THEN e2=expression ELSE e3=expression  { IfThenElse(e1,e2,e3) }
| IF e1=expression THEN e2=expression                     { IfThenElse(e1,e2,Unit)}

| LET m = motif EQUAL e1 = exprseq IN e2=exprseq { LetIn(m, e1, e2, false) }
| LET m = motif EQUAL e1 = exprseq DOUBLESEPARATOR e2=exprseq { LetIn(m, e1, e2, true) }
| LET REC m = motif EQUAL e=exprseq IN e2=exprseq          { LetIn(m, e, e2, false) } (*Rec utilisé inutilement*)


| FUN v=multivariables RIGHTARROW e=exprseq                { aux v e}
| LET v = VAR vs = multivariables EQUAL e=exprseq          { LetIn(MVar v, aux vs e, Var v, false) } 
| LET REC v = VAR vs = multivariables EQUAL e=exprseq          { LetRecIn(Var v, aux vs e, Var v, false) }
| LET v = VAR vs = multivariables EQUAL e=exprseq IN e2=exprseq          { LetIn(MVar v, aux vs e, e2, false) }
| LET REC v = VAR vs = multivariables EQUAL e=exprseq IN e2=exprseq          { LetRecIn(Var v, aux vs e, e2, false) }


| a=applic ASSIGN e=expression             { Assign(a, e) }

| e=internalexpression COMMA u=uplets   { upletaux e u }


| MATCH e = expression WITH m = cases                       { match m with | MatchWith(_, l) -> MatchWith(e,l) | _ -> failwith "comportement innatendu de la grammaire lors du parsing d'un match" }
| MATCH e = expression WITH CASE m = cases                       { match m with | MatchWith(_, l) -> MatchWith(e,l) | _ -> failwith "comportement innatendu de la grammaire lors du parsing d'un match" }


| FUNCTION m = cases { match m with | MatchWith(_, l) -> varanonyme := !varanonyme + 1 ; Fun(MVar ("$" ^ string_of_int !varanonyme), MatchWith(Var ("$" ^ string_of_int !varanonyme),l)) | _ -> failwith "comportement innatendu de la grammaire lors du parsing d'une fonction par filtrage" }
| FUNCTION CASE m = cases { match m with | MatchWith(_, l) -> varanonyme := !varanonyme + 1 ; Fun(MVar ("$" ^ string_of_int !varanonyme), MatchWith(Var ("$" ^ string_of_int !varanonyme),l)) | _ -> failwith "comportement innatendu de la grammaire lors du parsing d'une fonction par filtrage" }

| TRY e = exprseq WITH CASE EXCEPT m = motif RIGHTARROW en=exprseq {TryWith(e,m,en)}
| TRY e = exprseq WITH EXCEPT m = motif RIGHTARROW en=exprseq {TryWith(e,m,en)}

| TRY e = exprseq WITH CASE LPAREN EXCEPT m = motif RPAREN RIGHTARROW en=exprseq {TryWith(e,m,en)}
| TRY e = exprseq WITH LPAREN EXCEPT m = motif RPAREN RIGHTARROW en=exprseq {TryWith(e,m,en)}

(*retravailler pour utiliser les type*)
| RAISE LPAREN EXCEPT e = sexpr RPAREN                                                  {Raise(e)}

| e=internalexpression QUATROSPUNTOS l=listexpr { listaux e l }

| i=internalexpression { i }




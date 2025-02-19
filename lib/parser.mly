%{
(* --- PARTIE 1, préambule : ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Cst of int | Add of expr*expr | Mul of expr*expr | Min of expr*expr *)

(*utile pour les fonctions à n variables*)
let rec aux (m : expr) (e : expr) : expr = (*affiche_expr m; affiche_expr e; print_newline() ;*) match m with
|Fun(v, t) -> Fun(v, aux t e)
|LetIn (v,Unit,Unit,_) -> Fun(v, e)
|_ -> failwith "comportement innatendu de la grammaire lors du parsing d'une fonction"

let upletlist (e : expr) (u : expr) : expr = match u with
|Uplet(l) -> Uplet(e::l)
|List(l) -> List(e::l)
|_-> failwith "comportement innatendu de la grammaire lors du parsing d'un uplet"

let mupletlist (e : motif) (u : motif) : motif = match u with
|MUplet(l) -> MUplet(e::l)
|MCons(a,l)->MCons(e,MCons(a,l))
|_-> failwith "comportement innatendu de la grammaire lors du parsing d'un motif d'uplet"

let matchwithconstr ((m,v) : (motif * expr)) (matching : expr)= match matching with
|MatchWith(c,l) -> MatchWith(c, (m,v)::l)
|_-> failwith "comportement innatendu de la grammaire lors du parsing d'un match"

let varanonyme = ref 0

%}



/* PARTIE 2, on liste les lexèmes (lien avec le fichier lexer.mll) ******* */                                   
%token <int> INT       /* le lexème INT a un attribut entier */
%token <bool> BOOL
%token <string> VAR
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
%token FOR WHILE TO DO DONE
%token QUATROSPUNTOS LBRACKET RBRACKET
%token MATCH WITH CASE
%token LEQ LT GEQ GT DIFF
%token TRY EXCEPT RAISE

%token UNDERPLUS (*token factice ?*)

%token EOF             /* retour à la ligne */


/* PARTIE 3, on donne les associativités ********************************* */  


%left RIGHTARROW
%left CASE
%left EQUAL

%left THEN
%left ELSE

%left LEQ LT GEQ GT DIFF

%left REF


%left IN
%left SEPARATOR

%left ASSIGN

%nonassoc UNDERPLUS
%left PLUS MINUS AND OR   /* associativité gauche: a+b+c, c'est (a+b)+c */

   /* priorité plus grande de TIMES par rapport à
      PLUS et MINUS, car est sur une ligne située plus bas */
%left TIMES DIV NOT
%left DOUBLESEPARATOR
%left PRINT

/* PARTIE 4, le point d'entrée ******************************************* */
		    
%start main             /* "start" signale le point d'entrée du parser: */
                        /* c'est ici le non-terminal "main", qui est défini plus bas */
%type <Expr.expr> main     /* on _doit_ donner le type associé au point d'entrée "main" */


/* PARTIE 5 : la grammaire, enfin ! ************************************** */                                                         
%%

main:                       /* <- le point d'entrée (cf. + haut, "start") */
e=expression EOF { e }  /* on reconnaît une expression suivie de "EndOfLine", on la renvoie telle quelle */
  

/* règles de grammaire pour les expressions ; le non-terminal s'appelle "expression" */        

exprseq:
|e=expression %prec UNDERPLUS                    { e }
|e1=exprseq SEPARATOR e2=exprseq                 { LetIn(MVar "_",e1,e2,false)}


value:
  | i=INT                              { Cst i} 
      /* on appelle i l'attribut associé à INT */
      /* les "let in" sont juste là pour illustrer le fait que l'on peut mettre
         du code Caml dans les parties entre {..} ; supprimez-les pour
         montrer que vous avez lu ceci, et mettez juste "Cst i" */
   | b=BOOL                            { Bool b }
   | LPAREN RPAREN                     { Unit }



applic:
| a=applic s=sexpr { FunCall(a, s) } (*cas où on réduit le dernier paramètre*)
| v=VAR        { Var v } (*cas de base, il nous reste plus que le nom de la première fonction*)
| BANG e=sexpr s=sexpr                           { FunCall(Access(e),s) }
| LPAREN e=exprseq RPAREN s=sexpr { FunCall(e,s) }



sexpr:
| v = value                  { v }
| v = VAR                    { Var(v) }
| LPAREN e=exprseq RPAREN { e }       
| e = sexprlist QUATROSPUNTOS l = listexpr {upletlist e l }
| LBRACKET l = listexprbracket               { l }
| LBRACKET RBRACKET                    {List([])}


multivariables:
| mot=smotif m = multivariables { Fun(mot, m)}
| mot=smotif {LetIn(mot, Unit, Unit,false)} (*Pour des raisons de typage on utilise cette structure, elle n'a aucun sens en réalité*)

muplets:
| m=smotif COMMA u = muplets {mupletlist m u} 
| m=smotif            {MUplet([m])}

smotif :
|v = VAR                            {MVar(v)}
|LBRACKET RBRACKET                  {MNil}
|LPAREN m = motif RPAREN            {m}

motif:
|v = VAR                      {MVar(v)}
|m = smotif COMMA u=muplets   {mupletlist m u}
|LPAREN m=motif RPAREN   { m }
|m = smotif QUATROSPUNTOS l=smotif {MCons(m,l)}
|LBRACKET RBRACKET {MNil}

uplets:
| e=sexpr COMMA u = uplets  {upletlist e u}
| a=applic e=sexpr COMMA u = uplets  {upletlist (FunCall(a,e)) u}
| e=sexpr                  {Uplet([e])}
| a=applic e=sexpr                 {Uplet([FunCall(a,e)])}

sexprlist:
| v = value                  { v }
| v = VAR                    { Var(v) }
| LPAREN e=exprseq RPAREN { e }       

listexpr:
| e = sexprlist QUATROSPUNTOS l = listexpr { upletlist e l }
| LBRACKET RBRACKET                    { List([]) }

listexprbracket:
| e = sexprlist SEPARATOR l = listexprbracket { upletlist e l }
| e = sexprlist RBRACKET               { List([e])}

cases :
| m = motif RIGHTARROW e = exprseq CASE c = cases    { matchwithconstr (m,e) c }
| m = motif RIGHTARROW e = exprseq              { MatchWith(Unit, [(m,e)]) } (*On met Unit ici tant qu'on ne sait pas ce qu'on va match*)


expression:			   
   | v=value                               { v }

   | LPAREN e=exprseq RPAREN               { e }


   | e1=expression PLUS e2=expression      { Add(e1,e2) }
   | e1=expression TIMES e2=expression     { Mul(e1,e2) }
   | e1=expression MINUS e2=expression     { Min(e1,e2) }
   | e1=expression DIV e2=expression       { Div(e1,e2) }

   | e1=expression AND e2=expression       { And(e1,e2) }
   | e1=expression OR e2=expression        { Or(e1,e2) }
   | NOT e=expression                      { Not(e) }

   | e1=expression EQUAL e2=expression     { Equal(e1,e2) }
   | e1=expression LEQ e2=expression       { Leq(e1,e2) }
   | e1=expression LT e2=expression        { Lt(e1,e2) }
   | e1=expression GEQ e2=expression       { Not(Lt(e1,e2)) }
   | e1=expression GT e2=expression        { Not(Leq(e1,e2)) }
   | e1=expression DIFF e2=expression      { Not(Equal(e1,e2)) }
   
   | IF e1=expression THEN e2=expression ELSE e3=expression  { IfThenElse(e1,e2,e3) }
   | IF e1=expression THEN e2=expression                     { IfThenElse(e1,e2,Unit)}
   
   | LET m = motif EQUAL e1 = exprseq IN e2=exprseq { LetIn(m, e1, e2, false) }
   | LET m = motif EQUAL e1 = exprseq DOUBLESEPARATOR e2=expression { LetIn(m, e1, e2, true) }
   | LET REC m = motif EQUAL e=exprseq IN e2=exprseq          { LetIn(m, e, e2, false) } (*Rec utilisé inutilement*)
   
   (*| LET LPAREN e=expression COMMA u=uplets EQUAL e1 = expression IN e2=expression { LetIn(upletlist e u, e1, e2) }*)
   
   (*Je ne suis pas convaincu de l'efficacité de cette méthode, mais elle a le mérite de fonctionner*)
   | FUN v=multivariables RIGHTARROW e=exprseq                { aux v e}
   | LET v = VAR vs = multivariables EQUAL e=exprseq          { LetIn(MVar v, aux vs e, Var v, false) } 
   | LET REC v = VAR vs = multivariables EQUAL e=exprseq          { LetRecIn(Var v, aux vs e, Var v, false) }
   | LET v = VAR vs = multivariables EQUAL e=exprseq IN e2=exprseq          { LetIn(MVar v, aux vs e, e2, false) }
   | LET REC v = VAR vs = multivariables EQUAL e=exprseq IN e2=exprseq          { LetRecIn(Var v, aux vs e, e2, false) }


   | MINUS e=expression                    { Min(Cst 0, e) } (* le moins unaire *)  
   | PRINT e=expression                    { PrInt e } 
   | REF e=expression                      { Ref e }
   | BANG e=sexpr                            { Access(e) }
   | v=VAR ASSIGN e=expression             { Assign(Var v, e) }

   | e=sexpr COMMA u=uplets    { upletlist e u }
   | a=applic e=sexpr COMMA u=uplets    { upletlist (FunCall(a,e)) u }

   | FOR v=VAR EQUAL val1 = value TO val2=value DO e = expression DONE { For(Var v, val1, val2, e) }


   | WHILE b = expression DO e = expression DONE                                   { While(b,e)}

   | e = sexprlist QUATROSPUNTOS l = listexpr {upletlist e l }
   | LBRACKET l = listexprbracket               { l }
   | LBRACKET RBRACKET                    {List([])}

  
   | MATCH e = expression WITH m = cases                       { match m with | MatchWith(_, l) -> MatchWith(e,l) | _ -> failwith "comportement innatendu de la grammaire lors du parsing d'un match" }
   | MATCH e = expression WITH CASE m = cases                       { match m with | MatchWith(_, l) -> MatchWith(e,l) | _ -> failwith "comportement innatendu de la grammaire lors du parsing d'un match" }


   | FUNCTION m = cases { match m with | MatchWith(_, l) -> varanonyme := !varanonyme + 1 ; Fun(MVar ("$" ^ string_of_int !varanonyme), MatchWith(Var ("$" ^ string_of_int !varanonyme),l)) | _ -> failwith "comportement innatendu de la grammaire lors du parsing d'une fonction par filtrage" }
   | FUNCTION CASE m = cases { match m with | MatchWith(_, l) -> varanonyme := !varanonyme + 1 ; Fun(MVar ("$" ^ string_of_int !varanonyme), MatchWith(Var ("$" ^ string_of_int !varanonyme),l)) | _ -> failwith "comportement innatendu de la grammaire lors du parsing d'une fonction par filtrage" }

   | TRY e = exprseq WITH CASE EXCEPT m = motif RIGHTARROW en=exprseq {TryWith(e,m,en)}
   | TRY e = exprseq WITH EXCEPT m = motif RIGHTARROW en=exprseq {TryWith(e,m,en)}
   
   | RAISE LPAREN EXCEPT e = sexpr RPAREN                                                  {Raise(e)}


   | a=applic                             { a }



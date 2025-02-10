%{
(* --- PARTIE 1, préambule : ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Cst of int | Add of expr*expr | Mul of expr*expr | Min of expr*expr *)

(*utile pour les fonctions à n variables*)
let rec aux (m : expr) (e : expr) : expr = match m with
|Fun(v, t) -> Fun(v, aux t e)
|Var v -> Fun(Var v, e)
|_ -> failwith "comportement innatendu de la grammaire"
%}



/* PARTIE 2, on liste les lexèmes (lien avec le fichier lexer.mll) ******* */                                   
%token <int> INT       /* le lexème INT a un attribut entier */
%token <bool> BOOL
%token <string> VAR
%token PLUS TIMES MINUS
%token AND OR NOT
%token LPAREN RPAREN
%token IF THEN ELSE
%token PRINT
%token LET IN EQUAL
%token FUN RIGHTARROW
%token REC
%token REF BANG ASSIGN
%token SEPARATOR
%token EOL             /* retour à la ligne */


/* PARTIE 3, on donne les associativités ********************************* */  

%left SEPARATOR

%left ASSIGN

%left RIGHTARROW
%left EQUAL
%left PLUS MINUS AND OR   /* associativité gauche: a+b+c, c'est (a+b)+c */

   /* priorité plus grande de TIMES par rapport à
      PLUS et MINUS, car est sur une ligne située plus bas */
%left TIMES NOT

%left THEN
%left ELSE

%left IN

%left PRINT
/* PARTIE 4, le point d'entrée ******************************************* */
		    
%start main             /* "start" signale le point d'entrée du parser: */
                        /* c'est ici le non-terminal "main", qui est défini plus bas */
%type <Expr.expr> main     /* on _doit_ donner le type associé au point d'entrée "main" */


/* PARTIE 5 : la grammaire, enfin ! ************************************** */                                                         
%%

main:                       /* <- le point d'entrée (cf. + haut, "start") */
e=expression EOL { e }  /* on reconnaît une expression suivie de "EndOfLine", on la renvoie telle quelle */
  

/* règles de grammaire pour les expressions ; le non-terminal s'appelle "expression" */        

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

sexpr:
| v = value                  { v }
| v = VAR                    { Var(v) }
| LPAREN e=expression RPAREN { e }       

multivariables:
| v = VAR m = multivariables { Fun(Var(v), m)}
| v = VAR {Var(v)}


expression:			   
   | v=value                               { v }  

   | LPAREN e=expression RPAREN            { e } 
   | e1=expression SEPARATOR e2=expression { Accumulation(e1, e2) }
   | e1=expression PLUS e2=expression      { Add(e1,e2) }
   | e1=expression TIMES e2=expression     { Mul(e1,e2) }
   | e1=expression MINUS e2=expression     { Min(e1,e2) }
   | e1=expression AND e2=expression       { And(e1,e2) }
   | e1=expression OR e2=expression        { Or(e1,e2) }
   | NOT e=expression                      { Not(e) }
   
   | IF e1=expression THEN e2=expression ELSE e3=expression  { IfThenElse(e1,e2,e3) }
   | IF e1=expression THEN e2=expression                     { IfThenElse(e1,e2,Unit)}
   | LET v=VAR EQUAL e1 = expression IN e2=expression { LetIn(Var(v), e1, e2) }
   
   (*Je ne suis pas convaincu de l'efficacité de cette méthode, mais elle a le mérite de fonctionner*)
   | FUN v=multivariables RIGHTARROW e=expression                { aux v e}
   | LET v = VAR vs = multivariables EQUAL e=expression          { LetIn(Var v, aux vs e, Var v) }
   | LET REC v = VAR vs = multivariables EQUAL e=expression          { LetRecIn(Var v, aux vs e, Var v) }
   | LET v = VAR vs = multivariables EQUAL e=expression IN e2=expression          { LetIn(Var v, aux vs e, e2) }
   | LET REC v = VAR vs = multivariables EQUAL e=expression IN e2=expression          { LetRecIn(Var v, aux vs e, e2) }

   | MINUS e=expression                    { Min(Cst 0, e) } (* le moins unaire *)  
   | PRINT e=expression                    { PrInt e } 
   | REF v=value                           { Ref v }
   | BANG v=VAR                            { Access(Var v) }
   | v=VAR ASSIGN e=expression             { Assign(Var v, e) }

   | a=applic                              { a }
   (*| v=VAR e2=expression                   { FunCall(Var v, e2) }*)



%{
(* --- PARTIE 1, préambule : ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Cst of int | Add of expr*expr | Mul of expr*expr | Min of expr*expr *)

%}



/* PARTIE 2, on liste les lexèmes (lien avec le fichier lexer.mll) ******* */                                   
%token <int> INT       /* le lexème INT a un attribut entier */
%token <bool> BOOL
%token PLUS TIMES MINUS
%token AND OR NOT
%token LPAREN RPAREN
%token IF THEN ELSE
%token EOL             /* retour à la ligne */


/* PARTIE 3, on donne les associativités ********************************* */                            
%left PLUS MINUS AND OR   /* associativité gauche: a+b+c, c'est (a+b)+c */

   /* priorité plus grande de TIMES par rapport à
      PLUS et MINUS, car est sur une ligne située plus bas */
%left TIMES NOT

%left ELSE
/* PARTIE 4, le point d'entrée ******************************************* */
		    
%start main             /* "start" signale le point d'entrée du parser: */
                        /* c'est ici le non-terminal "main", qui est défini plus bas */
%type <Expr.expr> main     /* on _doit_ donner le type associé au point d'entrée "main" */


/* PARTIE 5 : la grammaire, enfin ! ************************************** */                                                         
%%

main:                       /* <- le point d'entrée (cf. + haut, "start") */
e=expression EOL { e }  /* on reconnaît une expression suivie de "EndOfLine", on la renvoie telle quelle */
  

/* règles de grammaire pour les expressions ; le non-terminal s'appelle "expression" */                                                                                
expression:			   
  | i=INT                             { Cst i} 
      /* on appelle i l'attribut associé à INT */
      /* les "let in" sont juste là pour illustrer le fait que l'on peut mettre
         du code Caml dans les parties entre {..} ; supprimez-les pour
         montrer que vous avez lu ceci, et mettez juste "Cst i" */
   | b=BOOL                            { Bool b }

   | LPAREN e=expression RPAREN            { e } 
   | e1=expression PLUS e2=expression      { Add(e1,e2) }
   | e1=expression TIMES e2=expression     { Mul(e1,e2) }
   | e1=expression MINUS e2=expression     { Min(e1,e2) }
   | e1=expression AND e2=expression       { And(e1,e2) }
   | e1=expression OR e2=expression        { Or(e1,e2) }
   | NOT e=expression                     { Not(e) }
   
   | IF e1=expression THEN e2=expression ELSE e3=expression { IfThenElse(e1,e2,e3) }

   | MINUS e=expression                    { Min(Cst 0, e) } (* le moins unaire *)  



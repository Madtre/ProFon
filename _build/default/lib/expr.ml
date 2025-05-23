
(*Ajouter let () = unit et fun () -> ..*)



let debug_mode = ref false

let src_mode = ref false

let noninf_mode = ref false

let type_mode = ref false



(*A defaut de mieux pour l'instant, on log les warnings avec un print*)
let warning (mess : string) = if !debug_mode then (print_string "Warning : "; print_string mess; print_newline());;

type supportedtype =
| Int
| Boolean
| Unit
| CustomType of string
| ProductType of supportedtype list
| EmptyType
(*
| List of supportedtype list
*)

type typedef =
| TType of string * supportedtype
and
customtype = string * typedef list

type motif =
|MVar of string
|MUplet of motif list
|MNil
|MCons of motif * motif
|MCustom of string * motif

(* un type pour des expressions arithm�tiques simples *)
type expr =
  Cst of int
| Bool of bool
| Var of string
| Unit
| Add of expr*expr
| Mul of expr*expr
| Div of expr*expr
| Min of expr*expr
| And of expr*expr
| Or of expr*expr
| Equal of expr*expr
| Leq of expr*expr
| Lt of expr*expr
| Not of expr
| IfThenElse of expr*expr*expr
| PrInt of expr
| LetIn of motif*expr*expr*bool
| LetRecIn of expr*expr*expr*bool
| Fun of motif*expr
| FunCall of expr*expr
| Ref of expr
| Access of expr
| Assign of expr*expr
| Uplet of expr list
| Nil
| Cons of expr*expr
| MatchWith of expr * ((motif * expr) list)
| TryWith of expr * motif * expr
| Raise of expr
| TypeDef of customtype*expr
| TypeUse of string*expr
| Exception of expr

(* les valeurs ; pour l'instant �a ne peut �tre que des entiers *)

type valeur = 
|VUnit
|VI of int
|VB of bool
|VFun of ((cont*cont)->valeur->valeur)
|VRef of string
|VUplet of valeur list
|VList of valeur list
|VException of valeur
|VCustom of string*valeur
and
cont = valeur -> valeur (*continuation*)

(*Fonctions permettant de cast des valeurs à un type utilisable*)
exception NotAnInt of valeur
exception NotABool of valeur
exception NotAFunction of valeur
exception NotARef of valeur
exception CastError of string

(* fonction d'affichage des valeurs *)
type env = (string, valeur) Hashtbl.t

exception NotAVariable of string
(*Gère l'erreur où on attendrait un nom de variable mais où une autre expression est donnée*)
exception NotComparable of valeur * valeur

  


module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | WITH
    | VAR of (
# 45 "lib/parser.mly"
       (string)
# 16 "lib/parser.ml"
  )
    | TYPE
    | TRY
    | TIMES
    | THEN
    | SEPARATOR
    | RPAREN
    | RIGHTARROW
    | REF
    | REC
    | RBRACKET
    | RAISE
    | QUATROSPUNTOS
    | PRINT
    | PLUS
    | OR
    | OF
    | NOT
    | MINUS
    | MATCH
    | LT
    | LPAREN
    | LET
    | LEQ
    | LBRACKET
    | INT of (
# 43 "lib/parser.mly"
       (int)
# 45 "lib/parser.ml"
  )
    | IN
    | IF
    | GT
    | GEQ
    | FUNCTION
    | FUN
    | EXCEPT
    | EQUAL
    | EOF
    | END
    | ELSE
    | DOUBLESEPARATOR
    | DIV
    | DIFF
    | CONSTRUCT of (
# 46 "lib/parser.mly"
       (string)
# 64 "lib/parser.ml"
  )
    | COMMA
    | CASE
    | BOOL of (
# 44 "lib/parser.mly"
       (bool)
# 71 "lib/parser.ml"
  )
    | BEGIN
    | BANG
    | ASSIGN
    | AND
  
end

include MenhirBasics

# 1 "lib/parser.mly"
  
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


# 121 "lib/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_main) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: main. *)

  | MenhirState002 : (('s, _menhir_box_main) _menhir_cell1_TYPE, _menhir_box_main) _menhir_state
    (** State 002.
        Stack shape : TYPE.
        Start symbol: main. *)

  | MenhirState004 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 004.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState006 : (('s, _menhir_box_main) _menhir_cell1_CONSTRUCT, _menhir_box_main) _menhir_state
    (** State 006.
        Stack shape : CONSTRUCT.
        Start symbol: main. *)

  | MenhirState008 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 008.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState011 : ((('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_CASE, _menhir_box_main) _menhir_state
    (** State 011.
        Stack shape : VAR CASE.
        Start symbol: main. *)

  | MenhirState014 : (('s, _menhir_box_main) _menhir_cell1_typecase, _menhir_box_main) _menhir_state
    (** State 014.
        Stack shape : typecase.
        Start symbol: main. *)

  | MenhirState018 : ((('s, _menhir_box_main) _menhir_cell1_TYPE, _menhir_box_main) _menhir_cell1_types, _menhir_box_main) _menhir_state
    (** State 018.
        Stack shape : TYPE types.
        Start symbol: main. *)

  | MenhirState021 : ((('s, _menhir_box_main) _menhir_cell1_TYPE, _menhir_box_main) _menhir_cell1_types, _menhir_box_main) _menhir_state
    (** State 021.
        Stack shape : TYPE types.
        Start symbol: main. *)

  | MenhirState022 : (((('s, _menhir_box_main) _menhir_cell1_TYPE, _menhir_box_main) _menhir_cell1_types, _menhir_box_main) _menhir_cell1_TYPE, _menhir_box_main) _menhir_state
    (** State 022.
        Stack shape : TYPE types TYPE.
        Start symbol: main. *)

  | MenhirState024 : (('s, _menhir_box_main) _menhir_cell1_TRY, _menhir_box_main) _menhir_state
    (** State 024.
        Stack shape : TRY.
        Start symbol: main. *)

  | MenhirState025 : (('s, _menhir_box_main) _menhir_cell1_REF, _menhir_box_main) _menhir_state
    (** State 025.
        Stack shape : REF.
        Start symbol: main. *)

  | MenhirState027 : (('s, _menhir_box_main) _menhir_cell1_RAISE, _menhir_box_main) _menhir_state
    (** State 027.
        Stack shape : RAISE.
        Start symbol: main. *)

  | MenhirState028 : (('s, _menhir_box_main) _menhir_cell1_PRINT, _menhir_box_main) _menhir_state
    (** State 028.
        Stack shape : PRINT.
        Start symbol: main. *)

  | MenhirState029 : (('s, _menhir_box_main) _menhir_cell1_NOT, _menhir_box_main) _menhir_state
    (** State 029.
        Stack shape : NOT.
        Start symbol: main. *)

  | MenhirState030 : (('s, _menhir_box_main) _menhir_cell1_MINUS, _menhir_box_main) _menhir_state
    (** State 030.
        Stack shape : MINUS.
        Start symbol: main. *)

  | MenhirState031 : (('s, _menhir_box_main) _menhir_cell1_LPAREN, _menhir_box_main) _menhir_state
    (** State 031.
        Stack shape : LPAREN.
        Start symbol: main. *)

  | MenhirState033 : (('s, _menhir_box_main) _menhir_cell1_MATCH, _menhir_box_main) _menhir_state
    (** State 033.
        Stack shape : MATCH.
        Start symbol: main. *)

  | MenhirState034 : (('s, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_state
    (** State 034.
        Stack shape : LET.
        Start symbol: main. *)

  | MenhirState035 : ((('s, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 035.
        Stack shape : LET VAR.
        Start symbol: main. *)

  | MenhirState037 : (('s, _menhir_box_main) _menhir_cell1_LPAREN, _menhir_box_main) _menhir_state
    (** State 037.
        Stack shape : LPAREN.
        Start symbol: main. *)

  | MenhirState039 : (('s, _menhir_box_main) _menhir_cell1_LPAREN, _menhir_box_main) _menhir_state
    (** State 039.
        Stack shape : LPAREN.
        Start symbol: main. *)

  | MenhirState042 : (('s, _menhir_box_main) _menhir_cell1_CONSTRUCT, _menhir_box_main) _menhir_state
    (** State 042.
        Stack shape : CONSTRUCT.
        Start symbol: main. *)

  | MenhirState047 : (('s, _menhir_box_main) _menhir_cell1_smotif, _menhir_box_main) _menhir_state
    (** State 047.
        Stack shape : smotif.
        Start symbol: main. *)

  | MenhirState049 : (('s, _menhir_box_main) _menhir_cell1_smotif, _menhir_box_main) _menhir_state
    (** State 049.
        Stack shape : smotif.
        Start symbol: main. *)

  | MenhirState053 : (('s, _menhir_box_main) _menhir_cell1_motifupletexpr, _menhir_box_main) _menhir_state
    (** State 053.
        Stack shape : motifupletexpr.
        Start symbol: main. *)

  | MenhirState056 : ((('s, _menhir_box_main) _menhir_cell1_motifupletexpr, _menhir_box_main) _menhir_cell1_motifupletexpr, _menhir_box_main) _menhir_state
    (** State 056.
        Stack shape : motifupletexpr motifupletexpr.
        Start symbol: main. *)

  | MenhirState062 : (('s, _menhir_box_main) _menhir_cell1_smotif, _menhir_box_main) _menhir_state
    (** State 062.
        Stack shape : smotif.
        Start symbol: main. *)

  | MenhirState065 : (((('s, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_multivariables, _menhir_box_main) _menhir_state
    (** State 065.
        Stack shape : LET VAR multivariables.
        Start symbol: main. *)

  | MenhirState066 : (('s, _menhir_box_main) _menhir_cell1_LBRACKET, _menhir_box_main) _menhir_state
    (** State 066.
        Stack shape : LBRACKET.
        Start symbol: main. *)

  | MenhirState069 : (('s, _menhir_box_main) _menhir_cell1_EXCEPT, _menhir_box_main) _menhir_state
    (** State 069.
        Stack shape : EXCEPT.
        Start symbol: main. *)

  | MenhirState072 : (('s, _menhir_box_main) _menhir_cell1_BEGIN, _menhir_box_main) _menhir_state
    (** State 072.
        Stack shape : BEGIN.
        Start symbol: main. *)

  | MenhirState073 : (('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_state
    (** State 073.
        Stack shape : IF.
        Start symbol: main. *)

  | MenhirState074 : (('s, _menhir_box_main) _menhir_cell1_FUNCTION, _menhir_box_main) _menhir_state
    (** State 074.
        Stack shape : FUNCTION.
        Start symbol: main. *)

  | MenhirState075 : ((('s, _menhir_box_main) _menhir_cell1_FUNCTION, _menhir_box_main) _menhir_cell1_CASE, _menhir_box_main) _menhir_state
    (** State 075.
        Stack shape : FUNCTION CASE.
        Start symbol: main. *)

  | MenhirState077 : (('s, _menhir_box_main) _menhir_cell1_motif, _menhir_box_main) _menhir_state
    (** State 077.
        Stack shape : motif.
        Start symbol: main. *)

  | MenhirState078 : (('s, _menhir_box_main) _menhir_cell1_FUN, _menhir_box_main) _menhir_state
    (** State 078.
        Stack shape : FUN.
        Start symbol: main. *)

  | MenhirState080 : ((('s, _menhir_box_main) _menhir_cell1_FUN, _menhir_box_main) _menhir_cell1_multivariables, _menhir_box_main) _menhir_state
    (** State 080.
        Stack shape : FUN multivariables.
        Start symbol: main. *)

  | MenhirState081 : (('s, _menhir_box_main) _menhir_cell1_BANG, _menhir_box_main) _menhir_state
    (** State 081.
        Stack shape : BANG.
        Start symbol: main. *)

  | MenhirState086 : (('s, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_state
    (** State 086.
        Stack shape : internalexpression.
        Start symbol: main. *)

  | MenhirState088 : (('s, _menhir_box_main) _menhir_cell1_applic, _menhir_box_main) _menhir_state
    (** State 088.
        Stack shape : applic.
        Start symbol: main. *)

  | MenhirState090 : (('s, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_state
    (** State 090.
        Stack shape : internalexpression.
        Start symbol: main. *)

  | MenhirState093 : ((('s, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_state
    (** State 093.
        Stack shape : internalexpression internalexpression.
        Start symbol: main. *)

  | MenhirState095 : (('s, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_state
    (** State 095.
        Stack shape : internalexpression.
        Start symbol: main. *)

  | MenhirState097 : (('s, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_state
    (** State 097.
        Stack shape : internalexpression.
        Start symbol: main. *)

  | MenhirState099 : (('s, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_state
    (** State 099.
        Stack shape : internalexpression.
        Start symbol: main. *)

  | MenhirState101 : (('s, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_state
    (** State 101.
        Stack shape : internalexpression.
        Start symbol: main. *)

  | MenhirState103 : (('s, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_state
    (** State 103.
        Stack shape : internalexpression.
        Start symbol: main. *)

  | MenhirState105 : (('s, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_state
    (** State 105.
        Stack shape : internalexpression.
        Start symbol: main. *)

  | MenhirState107 : (('s, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_state
    (** State 107.
        Stack shape : internalexpression.
        Start symbol: main. *)

  | MenhirState109 : (('s, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_state
    (** State 109.
        Stack shape : internalexpression.
        Start symbol: main. *)

  | MenhirState111 : (('s, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_state
    (** State 111.
        Stack shape : internalexpression.
        Start symbol: main. *)

  | MenhirState113 : (('s, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_state
    (** State 113.
        Stack shape : internalexpression.
        Start symbol: main. *)

  | MenhirState115 : (('s, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_state
    (** State 115.
        Stack shape : internalexpression.
        Start symbol: main. *)

  | MenhirState117 : (('s, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_state
    (** State 117.
        Stack shape : internalexpression.
        Start symbol: main. *)

  | MenhirState120 : ((('s, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_state
    (** State 120.
        Stack shape : internalexpression internalexpression.
        Start symbol: main. *)

  | MenhirState123 : (('s, _menhir_box_main) _menhir_cell1_exprseq, _menhir_box_main) _menhir_state
    (** State 123.
        Stack shape : exprseq.
        Start symbol: main. *)

  | MenhirState126 : (('s, _menhir_box_main) _menhir_cell1_applic, _menhir_box_main) _menhir_state
    (** State 126.
        Stack shape : applic.
        Start symbol: main. *)

  | MenhirState127 : ((('s, _menhir_box_main) _menhir_cell1_applic, _menhir_box_main) _menhir_cell1_ASSIGN, _menhir_box_main) _menhir_state
    (** State 127.
        Stack shape : applic ASSIGN.
        Start symbol: main. *)

  | MenhirState130 : ((('s, _menhir_box_main) _menhir_cell1_motif, _menhir_box_main) _menhir_cell1_exprseq, _menhir_box_main) _menhir_state
    (** State 130.
        Stack shape : motif exprseq.
        Start symbol: main. *)

  | MenhirState135 : ((('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 135.
        Stack shape : IF expression.
        Start symbol: main. *)

  | MenhirState137 : (((('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 137.
        Stack shape : IF expression expression.
        Start symbol: main. *)

  | MenhirState143 : (('s, _menhir_box_main) _menhir_cell1_sexprlistb, _menhir_box_main) _menhir_state
    (** State 143.
        Stack shape : sexprlistb.
        Start symbol: main. *)

  | MenhirState146 : (('s, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_state
    (** State 146.
        Stack shape : internalexpression.
        Start symbol: main. *)

  | MenhirState151 : ((((('s, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_multivariables, _menhir_box_main) _menhir_cell1_exprseq, _menhir_box_main) _menhir_state
    (** State 151.
        Stack shape : LET VAR multivariables exprseq.
        Start symbol: main. *)

  | MenhirState153 : ((('s, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_REC, _menhir_box_main) _menhir_state
    (** State 153.
        Stack shape : LET REC.
        Start symbol: main. *)

  | MenhirState154 : (((('s, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_REC, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 154.
        Stack shape : LET REC VAR.
        Start symbol: main. *)

  | MenhirState156 : ((((('s, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_REC, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_multivariables, _menhir_box_main) _menhir_state
    (** State 156.
        Stack shape : LET REC VAR multivariables.
        Start symbol: main. *)

  | MenhirState158 : (((((('s, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_REC, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_multivariables, _menhir_box_main) _menhir_cell1_exprseq, _menhir_box_main) _menhir_state
    (** State 158.
        Stack shape : LET REC VAR multivariables exprseq.
        Start symbol: main. *)

  | MenhirState161 : (((('s, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_REC, _menhir_box_main) _menhir_cell1_motif, _menhir_box_main) _menhir_state
    (** State 161.
        Stack shape : LET REC motif.
        Start symbol: main. *)

  | MenhirState163 : ((((('s, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_REC, _menhir_box_main) _menhir_cell1_motif, _menhir_box_main) _menhir_cell1_exprseq, _menhir_box_main) _menhir_state
    (** State 163.
        Stack shape : LET REC motif exprseq.
        Start symbol: main. *)

  | MenhirState166 : ((('s, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_motif, _menhir_box_main) _menhir_state
    (** State 166.
        Stack shape : LET motif.
        Start symbol: main. *)

  | MenhirState168 : (((('s, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_motif, _menhir_box_main) _menhir_cell1_exprseq, _menhir_box_main) _menhir_state
    (** State 168.
        Stack shape : LET motif exprseq.
        Start symbol: main. *)

  | MenhirState170 : (((('s, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_motif, _menhir_box_main) _menhir_cell1_exprseq, _menhir_box_main) _menhir_state
    (** State 170.
        Stack shape : LET motif exprseq.
        Start symbol: main. *)

  | MenhirState173 : ((('s, _menhir_box_main) _menhir_cell1_MATCH, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 173.
        Stack shape : MATCH expression.
        Start symbol: main. *)

  | MenhirState174 : (((('s, _menhir_box_main) _menhir_cell1_MATCH, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_cell1_CASE, _menhir_box_main) _menhir_state
    (** State 174.
        Stack shape : MATCH expression CASE.
        Start symbol: main. *)

  | MenhirState188 : ((('s, _menhir_box_main) _menhir_cell1_TRY, _menhir_box_main) _menhir_cell1_exprseq, _menhir_box_main) _menhir_state
    (** State 188.
        Stack shape : TRY exprseq.
        Start symbol: main. *)

  | MenhirState191 : (((('s, _menhir_box_main) _menhir_cell1_TRY, _menhir_box_main) _menhir_cell1_exprseq, _menhir_box_main) _menhir_cell1_motif, _menhir_box_main) _menhir_state
    (** State 191.
        Stack shape : TRY exprseq motif.
        Start symbol: main. *)

  | MenhirState193 : ((('s, _menhir_box_main) _menhir_cell1_TRY, _menhir_box_main) _menhir_cell1_exprseq, _menhir_box_main) _menhir_state
    (** State 193.
        Stack shape : TRY exprseq.
        Start symbol: main. *)

  | MenhirState195 : (((('s, _menhir_box_main) _menhir_cell1_TRY, _menhir_box_main) _menhir_cell1_exprseq, _menhir_box_main) _menhir_cell1_motif, _menhir_box_main) _menhir_state
    (** State 195.
        Stack shape : TRY exprseq motif.
        Start symbol: main. *)

  | MenhirState199 : ((('s, _menhir_box_main) _menhir_cell1_TRY, _menhir_box_main) _menhir_cell1_exprseq, _menhir_box_main) _menhir_state
    (** State 199.
        Stack shape : TRY exprseq.
        Start symbol: main. *)

  | MenhirState202 : (((('s, _menhir_box_main) _menhir_cell1_TRY, _menhir_box_main) _menhir_cell1_exprseq, _menhir_box_main) _menhir_cell1_motif, _menhir_box_main) _menhir_state
    (** State 202.
        Stack shape : TRY exprseq motif.
        Start symbol: main. *)

  | MenhirState204 : ((('s, _menhir_box_main) _menhir_cell1_TRY, _menhir_box_main) _menhir_cell1_exprseq, _menhir_box_main) _menhir_state
    (** State 204.
        Stack shape : TRY exprseq.
        Start symbol: main. *)

  | MenhirState206 : (((('s, _menhir_box_main) _menhir_cell1_TRY, _menhir_box_main) _menhir_cell1_exprseq, _menhir_box_main) _menhir_cell1_motif, _menhir_box_main) _menhir_state
    (** State 206.
        Stack shape : TRY exprseq motif.
        Start symbol: main. *)


and ('s, 'r) _menhir_cell1_applic = 
  | MenhirCell1_applic of 's * ('s, 'r) _menhir_state * (Expr.expr)

and ('s, 'r) _menhir_cell1_expression = 
  | MenhirCell1_expression of 's * ('s, 'r) _menhir_state * (Expr.expr)

and ('s, 'r) _menhir_cell1_exprseq = 
  | MenhirCell1_exprseq of 's * ('s, 'r) _menhir_state * (Expr.expr)

and ('s, 'r) _menhir_cell1_internalexpression = 
  | MenhirCell1_internalexpression of 's * ('s, 'r) _menhir_state * (Expr.expr)

and ('s, 'r) _menhir_cell1_motif = 
  | MenhirCell1_motif of 's * ('s, 'r) _menhir_state * (Expr.motif)

and ('s, 'r) _menhir_cell1_motifupletexpr = 
  | MenhirCell1_motifupletexpr of 's * ('s, 'r) _menhir_state * (Expr.motif)

and ('s, 'r) _menhir_cell1_multivariables = 
  | MenhirCell1_multivariables of 's * ('s, 'r) _menhir_state * (Expr.expr)

and ('s, 'r) _menhir_cell1_sexprlistb = 
  | MenhirCell1_sexprlistb of 's * ('s, 'r) _menhir_state * (Expr.expr)

and ('s, 'r) _menhir_cell1_smotif = 
  | MenhirCell1_smotif of 's * ('s, 'r) _menhir_state * (Expr.motif)

and ('s, 'r) _menhir_cell1_typecase = 
  | MenhirCell1_typecase of 's * ('s, 'r) _menhir_state * (Expr.typedef)

and ('s, 'r) _menhir_cell1_types = 
  | MenhirCell1_types of 's * ('s, 'r) _menhir_state * (Expr.customtype list)

and ('s, 'r) _menhir_cell1_ASSIGN = 
  | MenhirCell1_ASSIGN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_BANG = 
  | MenhirCell1_BANG of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_BEGIN = 
  | MenhirCell1_BEGIN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_CASE = 
  | MenhirCell1_CASE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_CONSTRUCT = 
  | MenhirCell1_CONSTRUCT of 's * ('s, 'r) _menhir_state * (
# 46 "lib/parser.mly"
       (string)
# 594 "lib/parser.ml"
)

and ('s, 'r) _menhir_cell1_EXCEPT = 
  | MenhirCell1_EXCEPT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_FUN = 
  | MenhirCell1_FUN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_FUNCTION = 
  | MenhirCell1_FUNCTION of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LBRACKET = 
  | MenhirCell1_LBRACKET of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LET = 
  | MenhirCell1_LET of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_MATCH = 
  | MenhirCell1_MATCH of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_MINUS = 
  | MenhirCell1_MINUS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_NOT = 
  | MenhirCell1_NOT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_PRINT = 
  | MenhirCell1_PRINT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_RAISE = 
  | MenhirCell1_RAISE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_REC = 
  | MenhirCell1_REC of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_REF = 
  | MenhirCell1_REF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_TRY = 
  | MenhirCell1_TRY of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_TYPE = 
  | MenhirCell1_TYPE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_VAR = 
  | MenhirCell1_VAR of 's * ('s, 'r) _menhir_state * (
# 45 "lib/parser.mly"
       (string)
# 649 "lib/parser.ml"
)

and _menhir_box_main = 
  | MenhirBox_main of (Expr.expr) [@@unboxed]

let _menhir_action_01 =
  fun a s ->
    (
# 156 "lib/parser.mly"
                   ( match a with |TypeUse(c,Uplet([])) -> TypeUse(c,s) |_ -> FunCall(a, s) )
# 660 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_02 =
  fun e ->
    (
# 157 "lib/parser.mly"
                 ( e )
# 668 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_03 =
  fun c e m ->
    (
# 211 "lib/parser.mly"
                                                     ( matchwithconstr (m,e) c )
# 676 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_04 =
  fun e m ->
    (
# 212 "lib/parser.mly"
                                                ( MatchWith(Unit, [(m,e)]) )
# 684 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_05 =
  fun e1 e2 e3 ->
    (
# 244 "lib/parser.mly"
                                                          ( IfThenElse(e1,e2,e3) )
# 692 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_06 =
  fun e1 e2 ->
    (
# 245 "lib/parser.mly"
                                                          ( IfThenElse(e1,e2,Unit))
# 700 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_07 =
  fun e1 e2 m ->
    (
# 247 "lib/parser.mly"
                                                 ( LetIn(m, e1, e2, false) )
# 708 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_08 =
  fun e1 e2 m ->
    (
# 248 "lib/parser.mly"
                                                              ( LetIn(m, e1, e2, true) )
# 716 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_09 =
  fun e e2 m ->
    (
# 249 "lib/parser.mly"
                                                           ( LetIn(m, e, e2, false) )
# 724 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_10 =
  fun e v ->
    (
# 252 "lib/parser.mly"
                                                           ( aux v e)
# 732 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_11 =
  fun e v vs ->
    (
# 253 "lib/parser.mly"
                                                           ( LetIn(MVar v, aux vs e, Var v, false) )
# 740 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_12 =
  fun e v vs ->
    (
# 254 "lib/parser.mly"
                                                               ( LetRecIn(Var v, aux vs e, Var v, false) )
# 748 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_13 =
  fun e e2 v vs ->
    (
# 255 "lib/parser.mly"
                                                                         ( LetIn(MVar v, aux vs e, e2, false) )
# 756 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_14 =
  fun e e2 v vs ->
    (
# 256 "lib/parser.mly"
                                                                             ( LetRecIn(Var v, aux vs e, e2, false) )
# 764 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_15 =
  fun a e ->
    (
# 259 "lib/parser.mly"
                                           ( Assign(a, e) )
# 772 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_16 =
  fun e u ->
    (
# 261 "lib/parser.mly"
                                        ( upletaux e u )
# 780 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_17 =
  fun e m ->
    (
# 264 "lib/parser.mly"
                                                            ( match m with | MatchWith(_, l) -> MatchWith(e,l) | _ -> failwith "comportement innatendu de la grammaire lors du parsing d'un match" )
# 788 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_18 =
  fun e m ->
    (
# 265 "lib/parser.mly"
                                                                 ( match m with | MatchWith(_, l) -> MatchWith(e,l) | _ -> failwith "comportement innatendu de la grammaire lors du parsing d'un match" )
# 796 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_19 =
  fun m ->
    (
# 268 "lib/parser.mly"
                     ( match m with | MatchWith(_, l) -> varanonyme := !varanonyme + 1 ; Fun(MVar ("$" ^ string_of_int !varanonyme), MatchWith(Var ("$" ^ string_of_int !varanonyme),l)) | _ -> failwith "comportement innatendu de la grammaire lors du parsing d'une fonction par filtrage" )
# 804 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_20 =
  fun m ->
    (
# 269 "lib/parser.mly"
                          ( match m with | MatchWith(_, l) -> varanonyme := !varanonyme + 1 ; Fun(MVar ("$" ^ string_of_int !varanonyme), MatchWith(Var ("$" ^ string_of_int !varanonyme),l)) | _ -> failwith "comportement innatendu de la grammaire lors du parsing d'une fonction par filtrage" )
# 812 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_21 =
  fun e en m ->
    (
# 271 "lib/parser.mly"
                                                                   (TryWith(e,m,en))
# 820 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_22 =
  fun e en m ->
    (
# 272 "lib/parser.mly"
                                                              (TryWith(e,m,en))
# 828 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_23 =
  fun e en m ->
    (
# 274 "lib/parser.mly"
                                                                                 (TryWith(e,m,en))
# 836 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_24 =
  fun e en m ->
    (
# 275 "lib/parser.mly"
                                                                            (TryWith(e,m,en))
# 844 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_25 =
  fun e l ->
    (
# 277 "lib/parser.mly"
                                                ( listaux e l )
# 852 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_26 =
  fun i ->
    (
# 279 "lib/parser.mly"
                       ( i )
# 860 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_27 =
  fun e ->
    (
# 139 "lib/parser.mly"
                                                 ( e )
# 868 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_28 =
  fun e1 e2 ->
    (
# 140 "lib/parser.mly"
                                                 ( LetIn(MVar "_",e1,e2,false))
# 876 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_29 =
  fun a ->
    (
# 215 "lib/parser.mly"
           (a)
# 884 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_30 =
  fun e1 e2 ->
    (
# 216 "lib/parser.mly"
                                                        ( Add(e1,e2) )
# 892 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_31 =
  fun e1 e2 ->
    (
# 217 "lib/parser.mly"
                                                        ( Mul(e1,e2) )
# 900 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_32 =
  fun e1 e2 ->
    (
# 218 "lib/parser.mly"
                                                        ( Min(e1,e2) )
# 908 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_33 =
  fun e1 e2 ->
    (
# 219 "lib/parser.mly"
                                                        ( Div(e1,e2) )
# 916 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_34 =
  fun e ->
    (
# 220 "lib/parser.mly"
                                                                 ( Min(Cst 0, e) )
# 924 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_35 =
  fun e1 e2 ->
    (
# 222 "lib/parser.mly"
                                                        ( And(e1,e2) )
# 932 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_36 =
  fun e1 e2 ->
    (
# 223 "lib/parser.mly"
                                                        ( Or(e1,e2) )
# 940 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_37 =
  fun e ->
    (
# 224 "lib/parser.mly"
                                                ( Not(e) )
# 948 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_38 =
  fun e1 e2 ->
    (
# 226 "lib/parser.mly"
                                                        ( Equal(e1,e2) )
# 956 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_39 =
  fun e1 e2 ->
    (
# 227 "lib/parser.mly"
                                                        ( Leq(e1,e2) )
# 964 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_40 =
  fun e1 e2 ->
    (
# 228 "lib/parser.mly"
                                                        ( Lt(e1,e2) )
# 972 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_41 =
  fun e1 e2 ->
    (
# 229 "lib/parser.mly"
                                                        ( Not(Lt(e1,e2)) )
# 980 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_42 =
  fun e1 e2 ->
    (
# 230 "lib/parser.mly"
                                                        ( Not(Leq(e1,e2)) )
# 988 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_43 =
  fun e1 e2 ->
    (
# 231 "lib/parser.mly"
                                                        ( Not(Equal(e1,e2)) )
# 996 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_44 =
  fun e ->
    (
# 233 "lib/parser.mly"
                                                        ( Ref e )
# 1004 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_45 =
  fun e ->
    (
# 234 "lib/parser.mly"
                                                ( PrInt e )
# 1012 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_46 =
  fun e ->
    (
# 237 "lib/parser.mly"
                                                                                    (Raise(e))
# 1020 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_47 =
  fun e l ->
    (
# 203 "lib/parser.mly"
                                                    ( listaux e l )
# 1028 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_48 =
  fun e ->
    (
# 204 "lib/parser.mly"
                                           ( List([e]) )
# 1036 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_49 =
  fun e l ->
    (
# 207 "lib/parser.mly"
                                               ( listaux e l )
# 1044 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_50 =
  fun e ->
    (
# 208 "lib/parser.mly"
                                        ( List(e::(List [])::[]))
# 1052 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_51 =
  fun e t ->
    (
# 108 "lib/parser.mly"
                                                ( List.fold_left (fun acc b -> TypeDef(b, acc)) e (List.rev t) )
# 1060 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_52 =
  fun t ->
    (
# 109 "lib/parser.mly"
                                   ( List.fold_left (fun acc b -> TypeDef(b, acc)) Unit t )
# 1068 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_53 =
  fun t ->
    (
# 110 "lib/parser.mly"
                   ( List.fold_left (fun acc b -> TypeDef(b, acc)) Unit t )
# 1076 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_54 =
  fun e ->
    (
# 111 "lib/parser.mly"
                  ( e )
# 1084 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_55 =
  fun v ->
    (
# 186 "lib/parser.mly"
                              (MVar(v))
# 1092 "lib/parser.ml"
     : (Expr.motif))

let _menhir_action_56 =
  fun c ->
    (
# 187 "lib/parser.mly"
                               (MCustom(c,MUplet([])))
# 1100 "lib/parser.ml"
     : (Expr.motif))

let _menhir_action_57 =
  fun c m ->
    (
# 188 "lib/parser.mly"
                               (MCustom(c,m))
# 1108 "lib/parser.ml"
     : (Expr.motif))

let _menhir_action_58 =
  fun m u ->
    (
# 189 "lib/parser.mly"
                                      (mupletlist m u)
# 1116 "lib/parser.ml"
     : (Expr.motif))

let _menhir_action_59 =
  fun l m ->
    (
# 190 "lib/parser.mly"
                                           (MCons(m,l))
# 1124 "lib/parser.ml"
     : (Expr.motif))

let _menhir_action_60 =
  fun m ->
    (
# 191 "lib/parser.mly"
                         ( m )
# 1132 "lib/parser.ml"
     : (Expr.motif))

let _menhir_action_61 =
  fun () ->
    (
# 192 "lib/parser.mly"
                   (MNil)
# 1140 "lib/parser.ml"
     : (Expr.motif))

let _menhir_action_62 =
  fun l m ->
    (
# 177 "lib/parser.mly"
                                            (MCons(m,l))
# 1148 "lib/parser.ml"
     : (Expr.motif))

let _menhir_action_63 =
  fun m ->
    (
# 178 "lib/parser.mly"
            ( m )
# 1156 "lib/parser.ml"
     : (Expr.motif))

let _menhir_action_64 =
  fun m mot ->
    (
# 169 "lib/parser.mly"
                                ( Fun(mot, m))
# 1164 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_65 =
  fun mot ->
    (
# 170 "lib/parser.mly"
             (LetIn(mot, Unit, Unit,false))
# 1172 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_66 =
  fun m u ->
    (
# 173 "lib/parser.mly"
                                     (mupletlist m u)
# 1180 "lib/parser.ml"
     : (Expr.motif))

let _menhir_action_67 =
  fun m ->
    (
# 174 "lib/parser.mly"
                              (MUplet([m]))
# 1188 "lib/parser.ml"
     : (Expr.motif))

let _menhir_action_68 =
  fun v ->
    (
# 161 "lib/parser.mly"
                             ( v )
# 1196 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_69 =
  fun e ->
    (
# 162 "lib/parser.mly"
                          ( e )
# 1204 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_70 =
  fun e ->
    (
# 163 "lib/parser.mly"
                      ( e )
# 1212 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_71 =
  fun l ->
    (
# 164 "lib/parser.mly"
                                             ( l )
# 1220 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_72 =
  fun e ->
    (
# 165 "lib/parser.mly"
                                          ( Access(e) )
# 1228 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_73 =
  fun a ->
    (
# 199 "lib/parser.mly"
                       (a)
# 1236 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_74 =
  fun e u ->
    (
# 200 "lib/parser.mly"
                                         ( upletaux e u )
# 1244 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_75 =
  fun v ->
    (
# 181 "lib/parser.mly"
                                    (MVar(v))
# 1252 "lib/parser.ml"
     : (Expr.motif))

let _menhir_action_76 =
  fun () ->
    (
# 182 "lib/parser.mly"
                                    (MNil)
# 1260 "lib/parser.ml"
     : (Expr.motif))

let _menhir_action_77 =
  fun m ->
    (
# 183 "lib/parser.mly"
                                    (m)
# 1268 "lib/parser.ml"
     : (Expr.motif))

let _menhir_action_78 =
  fun c ->
    (
# 127 "lib/parser.mly"
                (TType(c,EmptyType))
# 1276 "lib/parser.ml"
     : (Expr.typedef))

let _menhir_action_79 =
  fun c t ->
    (
# 128 "lib/parser.mly"
                                 ( 
    match t with
    |[]->failwith "erreur impossible"
    |[t]->TType(c,t)
    |l->TType(c,ProductType(l)) )
# 1288 "lib/parser.ml"
     : (Expr.typedef))

let _menhir_action_80 =
  fun l name ->
    (
# 119 "lib/parser.mly"
                                       ( (name, l) )
# 1296 "lib/parser.ml"
     : (Expr.customtype))

let _menhir_action_81 =
  fun l name ->
    (
# 120 "lib/parser.mly"
                                            ( (name, l) )
# 1304 "lib/parser.ml"
     : (Expr.customtype))

let _menhir_action_82 =
  fun c l ->
    (
# 123 "lib/parser.mly"
                                       ( c::l )
# 1312 "lib/parser.ml"
     : (Expr.typedef list))

let _menhir_action_83 =
  fun c ->
    (
# 124 "lib/parser.mly"
                                       ( c::[] )
# 1320 "lib/parser.ml"
     : (Expr.typedef list))

let _menhir_action_84 =
  fun t ->
    (
# 135 "lib/parser.mly"
                         ( (parsetype t)::[] )
# 1328 "lib/parser.ml"
     : (Expr.supportedtype list))

let _menhir_action_85 =
  fun l t ->
    (
# 136 "lib/parser.mly"
                            ( parsetype t::l )
# 1336 "lib/parser.ml"
     : (Expr.supportedtype list))

let _menhir_action_86 =
  fun t ->
    (
# 114 "lib/parser.mly"
            ( t::[] )
# 1344 "lib/parser.ml"
     : (Expr.customtype list))

let _menhir_action_87 =
  fun m t ->
    (
# 115 "lib/parser.mly"
                         ( t::m )
# 1352 "lib/parser.ml"
     : (Expr.customtype list))

let _menhir_action_88 =
  fun m t ->
    (
# 116 "lib/parser.mly"
                                         ( t::m )
# 1360 "lib/parser.ml"
     : (Expr.customtype list))

let _menhir_action_89 =
  fun a u ->
    (
# 195 "lib/parser.mly"
                                        ( upletaux a u)
# 1368 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_90 =
  fun a ->
    (
# 196 "lib/parser.mly"
                                        (Uplet([a]))
# 1376 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_91 =
  fun i ->
    (
# 144 "lib/parser.mly"
                                    ( Cst i)
# 1384 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_92 =
  fun b ->
    (
# 145 "lib/parser.mly"
                                    ( Bool b )
# 1392 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_93 =
  fun v ->
    (
# 146 "lib/parser.mly"
                                    ( Var v )
# 1400 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_94 =
  fun () ->
    (
# 147 "lib/parser.mly"
                                    ( Unit )
# 1408 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_95 =
  fun () ->
    (
# 148 "lib/parser.mly"
                                    ( List([]))
# 1416 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_96 =
  fun c ->
    (
# 149 "lib/parser.mly"
                                    ( TypeUse(c,Uplet[]) )
# 1424 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_97 =
  fun e ->
    (
# 150 "lib/parser.mly"
                                    ( Exception(e) )
# 1432 "lib/parser.ml"
     : (Expr.expr))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | AND ->
        "AND"
    | ASSIGN ->
        "ASSIGN"
    | BANG ->
        "BANG"
    | BEGIN ->
        "BEGIN"
    | BOOL _ ->
        "BOOL"
    | CASE ->
        "CASE"
    | COMMA ->
        "COMMA"
    | CONSTRUCT _ ->
        "CONSTRUCT"
    | DIFF ->
        "DIFF"
    | DIV ->
        "DIV"
    | DOUBLESEPARATOR ->
        "DOUBLESEPARATOR"
    | ELSE ->
        "ELSE"
    | END ->
        "END"
    | EOF ->
        "EOF"
    | EQUAL ->
        "EQUAL"
    | EXCEPT ->
        "EXCEPT"
    | FUN ->
        "FUN"
    | FUNCTION ->
        "FUNCTION"
    | GEQ ->
        "GEQ"
    | GT ->
        "GT"
    | IF ->
        "IF"
    | IN ->
        "IN"
    | INT _ ->
        "INT"
    | LBRACKET ->
        "LBRACKET"
    | LEQ ->
        "LEQ"
    | LET ->
        "LET"
    | LPAREN ->
        "LPAREN"
    | LT ->
        "LT"
    | MATCH ->
        "MATCH"
    | MINUS ->
        "MINUS"
    | NOT ->
        "NOT"
    | OF ->
        "OF"
    | OR ->
        "OR"
    | PLUS ->
        "PLUS"
    | PRINT ->
        "PRINT"
    | QUATROSPUNTOS ->
        "QUATROSPUNTOS"
    | RAISE ->
        "RAISE"
    | RBRACKET ->
        "RBRACKET"
    | REC ->
        "REC"
    | REF ->
        "REF"
    | RIGHTARROW ->
        "RIGHTARROW"
    | RPAREN ->
        "RPAREN"
    | SEPARATOR ->
        "SEPARATOR"
    | THEN ->
        "THEN"
    | TIMES ->
        "TIMES"
    | TRY ->
        "TRY"
    | TYPE ->
        "TYPE"
    | VAR _ ->
        "VAR"
    | WITH ->
        "WITH"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_goto_main : type  ttv_stack. ttv_stack -> _ -> _menhir_box_main =
    fun _menhir_stack _v ->
      MenhirBox_main _v
  
  let _menhir_run_213 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let e = _v in
          let _v = _menhir_action_54 e in
          _menhir_goto_main _menhir_stack _v
      | _ ->
          _eRR ()
  
  let _menhir_run_209 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_TYPE, _menhir_box_main) _menhir_cell1_types -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let MenhirCell1_types (_menhir_stack, _, t) = _menhir_stack in
          let MenhirCell1_TYPE (_menhir_stack, _) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_51 e t in
          _menhir_goto_main _menhir_stack _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_001 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let v = _v in
      let _v = _menhir_action_93 v in
      _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_value : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let v = _v in
      let _v = _menhir_action_68 v in
      _menhir_goto_sexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_sexpr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState069 ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState126 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState088 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState000 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState021 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState206 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState202 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState195 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState191 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState024 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState028 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState030 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState031 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState170 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState163 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState161 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState066 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState135 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState127 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState123 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState117 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState115 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState113 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState107 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState101 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState099 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState093 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState081 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_141 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_EXCEPT -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_EXCEPT (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_97 e in
      _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_089 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_applic -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_applic (_menhir_stack, _menhir_s, a) = _menhir_stack in
      let s = _v in
      let _v = _menhir_action_01 a s in
      _menhir_goto_applic _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_applic : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState000 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState021 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState024 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState206 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState202 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState195 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState191 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState031 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState170 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState161 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState163 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState135 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState127 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState123 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState028 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState030 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState066 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState117 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState113 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState115 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState107 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState101 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState099 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState093 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_126 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState126
      | LPAREN ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | INT _v_1 ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState126
      | EXCEPT ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | CONSTRUCT _v_2 ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState126
      | BOOL _v_3 ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState126
      | BEGIN ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | BANG ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | ASSIGN ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          let _menhir_stack = MenhirCell1_ASSIGN (_menhir_stack, MenhirState126) in
          let _menhir_s = MenhirState127 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRY ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUNCTION ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND | CASE | COMMA | DIFF | DIV | DOUBLESEPARATOR | ELSE | END | EOF | EQUAL | GEQ | GT | IN | LEQ | LT | MINUS | OR | PLUS | QUATROSPUNTOS | RPAREN | SEPARATOR | THEN | TIMES | WITH ->
          let a = _v in
          let _v = _menhir_action_29 a in
          _menhir_goto_internalexpression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_031 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState031
      | TRY ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_94 () in
          _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | REF ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | RAISE ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | PRINT ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | NOT ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | MINUS ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | MATCH ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | LPAREN ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | LET ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | INT _v ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState031
      | IF ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | FUNCTION ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | FUN ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | EXCEPT ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | CONSTRUCT _v ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState031
      | BOOL _v ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState031
      | BEGIN ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | BANG ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | _ ->
          _eRR ()
  
  and _menhir_run_024 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TRY (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState024 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRY ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUNCTION ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_025 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_REF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState025 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_026 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_RAISE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_s = MenhirState027 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRY ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUNCTION ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_028 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PRINT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState028 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_029 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_NOT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState029 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_030 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState030 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_066 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState066
      | REF ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | RBRACKET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_95 () in
          _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | RAISE ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | PRINT ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | NOT ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | MINUS ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | LPAREN ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | INT _v ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState066
      | EXCEPT ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | CONSTRUCT _v ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState066
      | BOOL _v ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState066
      | BEGIN ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | BANG ->
          let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | _ ->
          _eRR ()
  
  and _menhir_run_068 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let i = _v in
      let _v = _menhir_action_91 i in
      _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_069 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_EXCEPT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState069 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_070 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let c = _v in
      let _v = _menhir_action_96 c in
      _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_071 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let b = _v in
      let _v = _menhir_action_92 b in
      _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_072 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_BEGIN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState072 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRY ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUNCTION ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_033 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_MATCH (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState033 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRY ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUNCTION ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_034 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_0 ->
              let _menhir_stack = MenhirCell1_VAR (_menhir_stack, MenhirState034, _v) in
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState035
          | LPAREN ->
              let _menhir_stack = MenhirCell1_VAR (_menhir_stack, MenhirState034, _v) in
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
          | LBRACKET ->
              let _menhir_stack = MenhirCell1_VAR (_menhir_stack, MenhirState034, _v) in
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
          | EQUAL ->
              let _v =
                let v = _v in
                _menhir_action_55 v
              in
              _menhir_run_165 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
          | COMMA | QUATROSPUNTOS ->
              let _v =
                let v = _v in
                _menhir_action_75 v
              in
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
          | _ ->
              _eRR ())
      | REC ->
          let _menhir_stack = MenhirCell1_REC (_menhir_stack, MenhirState034) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v_1 ->
                  let _menhir_stack = MenhirCell1_VAR (_menhir_stack, MenhirState153, _v) in
                  _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState154
              | LPAREN ->
                  let _menhir_stack = MenhirCell1_VAR (_menhir_stack, MenhirState153, _v) in
                  _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState154
              | LBRACKET ->
                  let _menhir_stack = MenhirCell1_VAR (_menhir_stack, MenhirState153, _v) in
                  _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState154
              | EQUAL ->
                  let _v =
                    let v = _v in
                    _menhir_action_55 v
                  in
                  _menhir_run_160 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState153 _tok
              | COMMA | QUATROSPUNTOS ->
                  let _v =
                    let v = _v in
                    _menhir_action_75 v
                  in
                  _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState153 _tok
              | _ ->
                  _eRR ())
          | LPAREN ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
          | LBRACKET ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
          | CONSTRUCT _v ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState153
          | _ ->
              _eRR ())
      | LPAREN ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | LBRACKET ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | CONSTRUCT _v ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034
      | _ ->
          _eRR ()
  
  and _menhir_run_036 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let v = _v in
      let _v = _menhir_action_75 v in
      _menhir_goto_smotif _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_smotif : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState154 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState062 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState053 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState204 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState199 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState193 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState188 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState074 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState039 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState042 ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_062 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          let _menhir_stack = MenhirCell1_smotif (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState062
      | LPAREN ->
          let _menhir_stack = MenhirCell1_smotif (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_smotif (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | EQUAL | RIGHTARROW ->
          let mot = _v in
          let _v = _menhir_action_65 mot in
          _menhir_goto_multivariables _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_037 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState037 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_038 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RIGHTARROW | RPAREN ->
          let v = _v in
          let _v = _menhir_action_55 v in
          _menhir_goto_motif _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA | QUATROSPUNTOS ->
          let v = _v in
          let _v = _menhir_action_75 v in
          _menhir_goto_smotif _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_motif : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState204 ->
          _menhir_run_205 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState199 ->
          _menhir_run_200 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState193 ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState188 ->
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_165 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_160 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState074 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState039 ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_205 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_TRY, _menhir_box_main) _menhir_cell1_exprseq as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_motif (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RIGHTARROW ->
          let _menhir_s = MenhirState206 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRY ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUNCTION ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_073 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState073 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRY ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUNCTION ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_074 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FUNCTION (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState074 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CASE ->
          let _menhir_stack = MenhirCell1_CASE (_menhir_stack, _menhir_s) in
          let _menhir_s = MenhirState075 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_039 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState039 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_040 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RBRACKET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | EQUAL | RIGHTARROW | RPAREN ->
              let _v = _menhir_action_61 () in
              _menhir_goto_motif _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | COMMA | QUATROSPUNTOS ->
              let _v = _menhir_action_76 () in
              _menhir_goto_smotif _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_042 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          let _menhir_stack = MenhirCell1_CONSTRUCT (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState042
      | LPAREN ->
          let _menhir_stack = MenhirCell1_CONSTRUCT (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_CONSTRUCT (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
      | EQUAL | RIGHTARROW | RPAREN ->
          let c = _v in
          let _v = _menhir_action_56 c in
          _menhir_goto_motif _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_043 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RBRACKET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_76 () in
          _menhir_goto_smotif _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_078 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FUN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState078 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_081 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_BANG (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState081 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_200 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_TRY, _menhir_box_main) _menhir_cell1_exprseq as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_motif (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | RIGHTARROW ->
              let _menhir_s = MenhirState202 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TRY ->
                  _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | REF ->
                  _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | RAISE ->
                  _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PRINT ->
                  _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | NOT ->
                  _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MINUS ->
                  _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MATCH ->
                  _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LET ->
                  _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LBRACKET ->
                  _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT _v ->
                  _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IF ->
                  _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FUNCTION ->
                  _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FUN ->
                  _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | EXCEPT ->
                  _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CONSTRUCT _v ->
                  _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BOOL _v ->
                  _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BEGIN ->
                  _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BANG ->
                  _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_194 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_TRY, _menhir_box_main) _menhir_cell1_exprseq as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_motif (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RIGHTARROW ->
          let _menhir_s = MenhirState195 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRY ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUNCTION ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_189 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_TRY, _menhir_box_main) _menhir_cell1_exprseq as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_motif (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | RIGHTARROW ->
              let _menhir_s = MenhirState191 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TRY ->
                  _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | REF ->
                  _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | RAISE ->
                  _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PRINT ->
                  _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | NOT ->
                  _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MINUS ->
                  _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MATCH ->
                  _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LET ->
                  _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LBRACKET ->
                  _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT _v ->
                  _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IF ->
                  _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FUNCTION ->
                  _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FUN ->
                  _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | EXCEPT ->
                  _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CONSTRUCT _v ->
                  _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BOOL _v ->
                  _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BEGIN ->
                  _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BANG ->
                  _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_165 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_motif (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQUAL ->
          let _menhir_s = MenhirState166 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRY ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUNCTION ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_160 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_REC as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_motif (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQUAL ->
          let _menhir_s = MenhirState161 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRY ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUNCTION ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_076 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_motif (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RIGHTARROW ->
          let _menhir_s = MenhirState077 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRY ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUNCTION ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_060 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let m = _v in
          let _v = _menhir_action_77 m in
          _menhir_goto_smotif _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_058 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | EQUAL | RIGHTARROW | RPAREN ->
              let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
              let m = _v in
              let _v = _menhir_action_60 m in
              _menhir_goto_motif _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | COMMA | QUATROSPUNTOS ->
              let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
              let m = _v in
              let _v = _menhir_action_77 m in
              _menhir_goto_smotif _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_multivariables : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState154 ->
          _menhir_run_155 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState062 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_155 : type  ttv_stack. ((((ttv_stack, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_REC, _menhir_box_main) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_multivariables (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQUAL ->
          let _menhir_s = MenhirState156 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRY ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUNCTION ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_079 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_FUN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_multivariables (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RIGHTARROW ->
          let _menhir_s = MenhirState080 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRY ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUNCTION ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_064 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_multivariables (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQUAL ->
          let _menhir_s = MenhirState065 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRY ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUNCTION ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_063 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_smotif -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_smotif (_menhir_stack, _menhir_s, mot) = _menhir_stack in
      let m = _v in
      let _v = _menhir_action_64 m mot in
      _menhir_goto_multivariables _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_048 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | QUATROSPUNTOS ->
          let _menhir_stack = MenhirCell1_smotif (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState049 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | COMMA | EQUAL | RIGHTARROW | RPAREN ->
          let m = _v in
          let _v = _menhir_action_63 m in
          _menhir_goto_motifupletexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_motifupletexpr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState056 ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState053 ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState204 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState199 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState193 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState188 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState074 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState039 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState049 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_055 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_motifupletexpr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_motifupletexpr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState056 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | EQUAL | RIGHTARROW | RPAREN ->
          let m = _v in
          let _v = _menhir_action_67 m in
          _menhir_goto_muplets _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_muplets : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_motifupletexpr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState056 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState053 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_057 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_motifupletexpr, _menhir_box_main) _menhir_cell1_motifupletexpr -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_motifupletexpr (_menhir_stack, _menhir_s, m) = _menhir_stack in
      let u = _v in
      let _v = _menhir_action_66 m u in
      _menhir_goto_muplets _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_054 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_motifupletexpr -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_motifupletexpr (_menhir_stack, _menhir_s, m) = _menhir_stack in
      let u = _v in
      let _v = _menhir_action_58 m u in
      _menhir_goto_motif _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_052 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_motifupletexpr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_s = MenhirState053 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_051 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_smotif -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EQUAL | RIGHTARROW | RPAREN ->
          let MenhirCell1_smotif (_menhir_stack, _menhir_s, m) = _menhir_stack in
          let l = _v in
          let _v = _menhir_action_59 l m in
          _menhir_goto_motif _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA ->
          let MenhirCell1_smotif (_menhir_stack, _menhir_s, m) = _menhir_stack in
          let l = _v in
          let _v = _menhir_action_62 l m in
          _menhir_goto_motifupletexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_050 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_smotif -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_smotif (_menhir_stack, _menhir_s, m) = _menhir_stack in
      let l = _v in
      let _v = _menhir_action_62 l m in
      _menhir_goto_motifupletexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_046 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | QUATROSPUNTOS ->
          let _menhir_stack = MenhirCell1_smotif (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState047 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | COMMA ->
          let m = _v in
          let _v = _menhir_action_63 m in
          _menhir_goto_motifupletexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_045 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_CONSTRUCT -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_CONSTRUCT (_menhir_stack, _menhir_s, c) = _menhir_stack in
      let m = _v in
      let _v = _menhir_action_57 c m in
      _menhir_goto_motif _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_internalexpression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState025 ->
          _menhir_run_184 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState028 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState029 ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState030 ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState066 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState117 ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState115 ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState113 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState107 ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState101 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState099 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState095 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState093 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState000 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState021 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState206 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState202 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState195 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState191 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState024 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState031 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState170 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState163 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState161 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState135 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState127 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState123 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_184 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_REF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | COMMA | DIFF | DOUBLESEPARATOR | ELSE | END | EOF | EQUAL | GEQ | GT | IN | LEQ | LT | QUATROSPUNTOS | RBRACKET | RPAREN | SEPARATOR | THEN | WITH ->
          let MenhirCell1_REF (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_44 e in
          _menhir_goto_internalexpression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_086 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState086 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_095 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState095 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_099 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState099 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_101 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState101 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_097 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState097 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_105 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState105 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_181 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_PRINT -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_PRINT (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_45 e in
      _menhir_goto_internalexpression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_180 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_NOT -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_NOT (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_37 e in
      _menhir_goto_internalexpression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_179 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_MINUS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | CASE | COMMA | DIFF | DOUBLESEPARATOR | ELSE | END | EOF | EQUAL | GEQ | GT | IN | LEQ | LT | MINUS | OR | PLUS | QUATROSPUNTOS | RBRACKET | RPAREN | SEPARATOR | THEN | WITH ->
          let MenhirCell1_MINUS (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_34 e in
          _menhir_goto_internalexpression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_145 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GEQ ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUAL ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIFF ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState146 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RBRACKET | SEPARATOR ->
          let a = _v in
          let _v = _menhir_action_73 a in
          _menhir_goto_sexprlistb _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_103 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState103 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_107 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState107 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_109 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState109 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_111 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState111 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_113 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState113 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_115 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState115 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_sexprlistb : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_sexprlistb (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState143 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | RBRACKET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let e = _v in
          let _v = _menhir_action_50 e in
          _menhir_goto_listexprbracket _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_listexprbracket : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState066 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState143 ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_149 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_LBRACKET -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) = _menhir_stack in
      let l = _v in
      let _v = _menhir_action_71 l in
      _menhir_goto_sexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_144 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_sexprlistb -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_sexprlistb (_menhir_stack, _menhir_s, e) = _menhir_stack in
      let l = _v in
      let _v = _menhir_action_49 e l in
      _menhir_goto_listexprbracket _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_119 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GEQ ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUAL ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIFF ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState120 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | DOUBLESEPARATOR | ELSE | END | EOF | IN | RBRACKET | RPAREN | SEPARATOR | THEN | WITH ->
          let a = _v in
          let _v = _menhir_action_90 a in
          _menhir_goto_uplets _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_uplets : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState146 ->
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState120 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState117 ->
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_147 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_internalexpression (_menhir_stack, _menhir_s, e) = _menhir_stack in
      let u = _v in
      let _v = _menhir_action_74 e u in
      _menhir_goto_sexprlistb _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_121 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_cell1_internalexpression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_internalexpression (_menhir_stack, _menhir_s, a) = _menhir_stack in
      let u = _v in
      let _v = _menhir_action_89 a u in
      _menhir_goto_uplets _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_118 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_internalexpression (_menhir_stack, _menhir_s, e) = _menhir_stack in
      let u = _v in
      let _v = _menhir_action_16 e u in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState000 ->
          _menhir_run_213 _menhir_stack _v _tok
      | MenhirState021 ->
          _menhir_run_209 _menhir_stack _v _tok
      | MenhirState027 ->
          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState033 ->
          _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_138 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState135 ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState127 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState024 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState206 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState202 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState195 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState191 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState031 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState170 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState161 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState163 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState123 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_182 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_RAISE -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_RAISE (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_46 e in
          _menhir_goto_internalexpression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_172 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_MATCH as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          let _menhir_s = MenhirState173 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CASE ->
              let _menhir_stack = MenhirCell1_CASE (_menhir_stack, _menhir_s) in
              let _menhir_s = MenhirState174 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | LPAREN ->
                  _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LBRACKET ->
                  _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CONSTRUCT _v ->
                  _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_138 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expression (_menhir_stack, _, e2) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, e1) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
      let e3 = _v in
      let _v = _menhir_action_05 e1 e2 e3 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_136 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState137 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRY ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUNCTION ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | CASE | DOUBLESEPARATOR | END | EOF | IN | RPAREN | SEPARATOR | THEN | WITH ->
          let MenhirCell1_expression (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_06 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_134 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | THEN ->
          let _menhir_s = MenhirState135 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRY ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUNCTION ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_128 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_applic, _menhir_box_main) _menhir_cell1_ASSIGN -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ASSIGN (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_applic (_menhir_stack, _menhir_s, a) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_15 a e in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_125 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let e = _v in
      let _v = _menhir_action_27 e in
      _menhir_goto_exprseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_exprseq : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState206 ->
          _menhir_run_207 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState202 ->
          _menhir_run_203 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState195 ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState191 ->
          _menhir_run_192 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState024 ->
          _menhir_run_185 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState031 ->
          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState170 ->
          _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_169 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState163 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState161 ->
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_152 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState123 ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_207 : type  ttv_stack. ((((ttv_stack, _menhir_box_main) _menhir_cell1_TRY, _menhir_box_main) _menhir_cell1_exprseq, _menhir_box_main) _menhir_cell1_motif as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | DOUBLESEPARATOR | ELSE | END | EOF | IN | RPAREN | THEN | WITH ->
          let MenhirCell1_motif (_menhir_stack, _, m) = _menhir_stack in
          let MenhirCell1_exprseq (_menhir_stack, _, e) = _menhir_stack in
          let MenhirCell1_TRY (_menhir_stack, _menhir_s) = _menhir_stack in
          let en = _v in
          let _v = _menhir_action_21 e en m in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_123 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_exprseq -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState123 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRY ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUNCTION ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_203 : type  ttv_stack. ((((ttv_stack, _menhir_box_main) _menhir_cell1_TRY, _menhir_box_main) _menhir_cell1_exprseq, _menhir_box_main) _menhir_cell1_motif as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | DOUBLESEPARATOR | ELSE | END | EOF | IN | RPAREN | THEN | WITH ->
          let MenhirCell1_motif (_menhir_stack, _, m) = _menhir_stack in
          let MenhirCell1_exprseq (_menhir_stack, _, e) = _menhir_stack in
          let MenhirCell1_TRY (_menhir_stack, _menhir_s) = _menhir_stack in
          let en = _v in
          let _v = _menhir_action_23 e en m in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_196 : type  ttv_stack. ((((ttv_stack, _menhir_box_main) _menhir_cell1_TRY, _menhir_box_main) _menhir_cell1_exprseq, _menhir_box_main) _menhir_cell1_motif as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | DOUBLESEPARATOR | ELSE | END | EOF | IN | RPAREN | THEN | WITH ->
          let MenhirCell1_motif (_menhir_stack, _, m) = _menhir_stack in
          let MenhirCell1_exprseq (_menhir_stack, _, e) = _menhir_stack in
          let MenhirCell1_TRY (_menhir_stack, _menhir_s) = _menhir_stack in
          let en = _v in
          let _v = _menhir_action_22 e en m in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_192 : type  ttv_stack. ((((ttv_stack, _menhir_box_main) _menhir_cell1_TRY, _menhir_box_main) _menhir_cell1_exprseq, _menhir_box_main) _menhir_cell1_motif as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | DOUBLESEPARATOR | ELSE | END | EOF | IN | RPAREN | THEN | WITH ->
          let MenhirCell1_motif (_menhir_stack, _, m) = _menhir_stack in
          let MenhirCell1_exprseq (_menhir_stack, _, e) = _menhir_stack in
          let MenhirCell1_TRY (_menhir_stack, _menhir_s) = _menhir_stack in
          let en = _v in
          let _v = _menhir_action_24 e en m in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_185 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_TRY as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | EXCEPT ->
                  let _menhir_s = MenhirState188 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | VAR _v ->
                      _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | LPAREN ->
                      _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LBRACKET ->
                      _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | CONSTRUCT _v ->
                      _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | EXCEPT ->
              let _menhir_s = MenhirState193 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | LPAREN ->
                  _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LBRACKET ->
                  _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CONSTRUCT _v ->
                  _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | CASE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LPAREN ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | EXCEPT ->
                      let _menhir_s = MenhirState199 in
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | VAR _v ->
                          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                      | LPAREN ->
                          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | LBRACKET ->
                          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                      | CONSTRUCT _v ->
                          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                      | _ ->
                          _eRR ())
                  | _ ->
                      _eRR ())
              | EXCEPT ->
                  let _menhir_s = MenhirState204 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | VAR _v ->
                      _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | LPAREN ->
                      _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LBRACKET ->
                      _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | CONSTRUCT _v ->
                      _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | SEPARATOR ->
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_177 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_69 e in
          _menhir_goto_sexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_171 : type  ttv_stack. ((((ttv_stack, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_motif, _menhir_box_main) _menhir_cell1_exprseq as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | DOUBLESEPARATOR | ELSE | END | EOF | IN | RPAREN | THEN | WITH ->
          let MenhirCell1_exprseq (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_motif (_menhir_stack, _, m) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_08 e1 e2 m in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_169 : type  ttv_stack. ((((ttv_stack, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_motif, _menhir_box_main) _menhir_cell1_exprseq as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | DOUBLESEPARATOR | ELSE | END | EOF | IN | RPAREN | THEN | WITH ->
          let MenhirCell1_exprseq (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_motif (_menhir_stack, _, m) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_07 e1 e2 m in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_167 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_motif as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEPARATOR ->
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IN ->
          let _menhir_s = MenhirState168 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRY ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUNCTION ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | DOUBLESEPARATOR ->
          let _menhir_s = MenhirState170 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRY ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUNCTION ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_164 : type  ttv_stack. (((((ttv_stack, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_REC, _menhir_box_main) _menhir_cell1_motif, _menhir_box_main) _menhir_cell1_exprseq as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | DOUBLESEPARATOR | ELSE | END | EOF | IN | RPAREN | THEN | WITH ->
          let MenhirCell1_exprseq (_menhir_stack, _, e) = _menhir_stack in
          let MenhirCell1_motif (_menhir_stack, _, m) = _menhir_stack in
          let MenhirCell1_REC (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_09 e e2 m in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_162 : type  ttv_stack. ((((ttv_stack, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_REC, _menhir_box_main) _menhir_cell1_motif as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEPARATOR ->
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IN ->
          let _menhir_s = MenhirState163 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRY ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUNCTION ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_159 : type  ttv_stack. ((((((ttv_stack, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_REC, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_multivariables, _menhir_box_main) _menhir_cell1_exprseq as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | DOUBLESEPARATOR | ELSE | END | EOF | IN | RPAREN | THEN | WITH ->
          let MenhirCell1_exprseq (_menhir_stack, _, e) = _menhir_stack in
          let MenhirCell1_multivariables (_menhir_stack, _, vs) = _menhir_stack in
          let MenhirCell1_VAR (_menhir_stack, _, v) = _menhir_stack in
          let MenhirCell1_REC (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_14 e e2 v vs in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_157 : type  ttv_stack. (((((ttv_stack, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_REC, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_multivariables as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IN ->
          let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState158 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRY ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUNCTION ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | CASE | DOUBLESEPARATOR | ELSE | END | EOF | RPAREN | THEN | WITH ->
          let MenhirCell1_multivariables (_menhir_stack, _, vs) = _menhir_stack in
          let MenhirCell1_VAR (_menhir_stack, _, v) = _menhir_stack in
          let MenhirCell1_REC (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_12 e v vs in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_152 : type  ttv_stack. (((((ttv_stack, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_multivariables, _menhir_box_main) _menhir_cell1_exprseq as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | DOUBLESEPARATOR | ELSE | END | EOF | IN | RPAREN | THEN | WITH ->
          let MenhirCell1_exprseq (_menhir_stack, _, e) = _menhir_stack in
          let MenhirCell1_multivariables (_menhir_stack, _, vs) = _menhir_stack in
          let MenhirCell1_VAR (_menhir_stack, _, v) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_13 e e2 v vs in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_150 : type  ttv_stack. ((((ttv_stack, _menhir_box_main) _menhir_cell1_LET, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_multivariables as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IN ->
          let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState151 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TRY ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUNCTION ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | CASE | DOUBLESEPARATOR | ELSE | END | EOF | RPAREN | THEN | WITH ->
          let MenhirCell1_multivariables (_menhir_stack, _, vs) = _menhir_stack in
          let MenhirCell1_VAR (_menhir_stack, _, v) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_11 e v vs in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_139 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_BEGIN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | END ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_BEGIN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_70 e in
          _menhir_goto_sexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_129 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_motif as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE ->
          let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState130 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | DOUBLESEPARATOR | ELSE | END | EOF | IN | RPAREN | THEN | WITH ->
          let MenhirCell1_motif (_menhir_stack, _menhir_s, m) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_04 e m in
          _menhir_goto_cases _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_cases : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState173 ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState174 ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState074 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState075 ->
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState130 ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_176 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_MATCH, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expression (_menhir_stack, _, e) = _menhir_stack in
      let MenhirCell1_MATCH (_menhir_stack, _menhir_s) = _menhir_stack in
      let m = _v in
      let _v = _menhir_action_17 e m in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_175 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_MATCH, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_cell1_CASE -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_CASE (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, e) = _menhir_stack in
      let MenhirCell1_MATCH (_menhir_stack, _menhir_s) = _menhir_stack in
      let m = _v in
      let _v = _menhir_action_18 e m in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_133 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_FUNCTION -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_FUNCTION (_menhir_stack, _menhir_s) = _menhir_stack in
      let m = _v in
      let _v = _menhir_action_19 m in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_132 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_FUNCTION, _menhir_box_main) _menhir_cell1_CASE -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_CASE (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_FUNCTION (_menhir_stack, _menhir_s) = _menhir_stack in
      let m = _v in
      let _v = _menhir_action_20 m in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_131 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_motif, _menhir_box_main) _menhir_cell1_exprseq -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_exprseq (_menhir_stack, _, e) = _menhir_stack in
      let MenhirCell1_motif (_menhir_stack, _menhir_s, m) = _menhir_stack in
      let c = _v in
      let _v = _menhir_action_03 c e m in
      _menhir_goto_cases _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_124 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_exprseq as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | DOUBLESEPARATOR | ELSE | END | EOF | IN | RPAREN | THEN | WITH ->
          let MenhirCell1_exprseq (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_28 e1 e2 in
          _menhir_goto_exprseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_122 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_FUN, _menhir_box_main) _menhir_cell1_multivariables as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_exprseq (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | DOUBLESEPARATOR | ELSE | END | EOF | IN | RPAREN | THEN | WITH ->
          let MenhirCell1_multivariables (_menhir_stack, _, v) = _menhir_stack in
          let MenhirCell1_FUN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_10 e v in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_116 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | COMMA | DIFF | DOUBLESEPARATOR | ELSE | END | EOF | EQUAL | GEQ | GT | IN | LEQ | LT | QUATROSPUNTOS | RBRACKET | RPAREN | SEPARATOR | THEN | WITH ->
          let MenhirCell1_internalexpression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_43 e1 e2 in
          _menhir_goto_internalexpression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_114 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GEQ ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIFF ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | COMMA | DOUBLESEPARATOR | ELSE | END | EOF | EQUAL | IN | QUATROSPUNTOS | RBRACKET | RPAREN | SEPARATOR | THEN | WITH ->
          let MenhirCell1_internalexpression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_38 e1 e2 in
          _menhir_goto_internalexpression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_112 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | COMMA | DIFF | DOUBLESEPARATOR | ELSE | END | EOF | EQUAL | GEQ | GT | IN | LEQ | LT | QUATROSPUNTOS | RBRACKET | RPAREN | SEPARATOR | THEN | WITH ->
          let MenhirCell1_internalexpression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_41 e1 e2 in
          _menhir_goto_internalexpression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_110 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | COMMA | DIFF | DOUBLESEPARATOR | ELSE | END | EOF | EQUAL | GEQ | GT | IN | LEQ | LT | QUATROSPUNTOS | RBRACKET | RPAREN | SEPARATOR | THEN | WITH ->
          let MenhirCell1_internalexpression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_42 e1 e2 in
          _menhir_goto_internalexpression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_108 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | COMMA | DIFF | DOUBLESEPARATOR | ELSE | END | EOF | EQUAL | GEQ | GT | IN | LEQ | LT | QUATROSPUNTOS | RBRACKET | RPAREN | SEPARATOR | THEN | WITH ->
          let MenhirCell1_internalexpression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_39 e1 e2 in
          _menhir_goto_internalexpression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_106 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | CASE | COMMA | DIFF | DOUBLESEPARATOR | ELSE | END | EOF | EQUAL | GEQ | GT | IN | LEQ | LT | MINUS | OR | PLUS | QUATROSPUNTOS | RBRACKET | RPAREN | SEPARATOR | THEN | WITH ->
          let MenhirCell1_internalexpression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_35 e1 e2 in
          _menhir_goto_internalexpression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_104 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | COMMA | DIFF | DOUBLESEPARATOR | ELSE | END | EOF | EQUAL | GEQ | GT | IN | LEQ | LT | QUATROSPUNTOS | RBRACKET | RPAREN | SEPARATOR | THEN | WITH ->
          let MenhirCell1_internalexpression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_40 e1 e2 in
          _menhir_goto_internalexpression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_102 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | CASE | COMMA | DIFF | DOUBLESEPARATOR | ELSE | END | EOF | EQUAL | GEQ | GT | IN | LEQ | LT | MINUS | OR | PLUS | QUATROSPUNTOS | RBRACKET | RPAREN | SEPARATOR | THEN | WITH ->
          let MenhirCell1_internalexpression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_32 e1 e2 in
          _menhir_goto_internalexpression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_100 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | CASE | COMMA | DIFF | DOUBLESEPARATOR | ELSE | END | EOF | EQUAL | GEQ | GT | IN | LEQ | LT | MINUS | OR | PLUS | QUATROSPUNTOS | RBRACKET | RPAREN | SEPARATOR | THEN | WITH ->
          let MenhirCell1_internalexpression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_36 e1 e2 in
          _menhir_goto_internalexpression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_098 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_internalexpression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_33 e1 e2 in
      _menhir_goto_internalexpression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_096 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | CASE | COMMA | DIFF | DOUBLESEPARATOR | ELSE | END | EOF | EQUAL | GEQ | GT | IN | LEQ | LT | MINUS | OR | PLUS | QUATROSPUNTOS | RBRACKET | RPAREN | SEPARATOR | THEN | WITH ->
          let MenhirCell1_internalexpression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_30 e1 e2 in
          _menhir_goto_internalexpression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_092 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | QUATROSPUNTOS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState093 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | PLUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GEQ ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUAL ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIFF ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | DOUBLESEPARATOR | ELSE | END | EOF | IN | RPAREN | SEPARATOR | THEN | WITH ->
          let e = _v in
          let _v = _menhir_action_48 e in
          _menhir_goto_listexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_listexpr : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState093 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState090 ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_094 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression, _menhir_box_main) _menhir_cell1_internalexpression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_internalexpression (_menhir_stack, _menhir_s, e) = _menhir_stack in
      let l = _v in
      let _v = _menhir_action_47 e l in
      _menhir_goto_listexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_091 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_internalexpression (_menhir_stack, _menhir_s, e) = _menhir_stack in
      let l = _v in
      let _v = _menhir_action_25 e l in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_087 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_internalexpression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_internalexpression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_31 e1 e2 in
      _menhir_goto_internalexpression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_085 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | QUATROSPUNTOS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState090 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | PLUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GEQ ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUAL ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIFF ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState117 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REF ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RAISE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACKET ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | EXCEPT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTRUCT _v ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BEGIN ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND ->
          let _menhir_stack = MenhirCell1_internalexpression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CASE | DOUBLESEPARATOR | ELSE | END | EOF | IN | RPAREN | SEPARATOR | THEN | WITH ->
          let i = _v in
          let _v = _menhir_action_26 i in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_088 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState088
      | LPAREN ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | INT _v_1 ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState088
      | EXCEPT ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | CONSTRUCT _v_2 ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState088
      | BOOL _v_3 ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState088
      | BEGIN ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | BANG ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | AND | CASE | COMMA | DIFF | DIV | DOUBLESEPARATOR | ELSE | END | EOF | EQUAL | GEQ | GT | IN | LEQ | LT | MINUS | OR | PLUS | QUATROSPUNTOS | RBRACKET | RPAREN | SEPARATOR | THEN | TIMES | WITH ->
          let a = _v in
          let _v = _menhir_action_29 a in
          _menhir_goto_internalexpression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_084 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let e = _v in
      let _v = _menhir_action_02 e in
      _menhir_goto_applic _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_083 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_BANG -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_BANG (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_72 e in
      _menhir_goto_sexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let rec _menhir_run_003 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | EQUAL ->
          let _menhir_s = MenhirState004 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | CONSTRUCT _v ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CASE ->
              let _menhir_stack = MenhirCell1_CASE (_menhir_stack, _menhir_s) in
              let _menhir_s = MenhirState011 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | CONSTRUCT _v ->
                  _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_005 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | OF ->
          let _menhir_stack = MenhirCell1_CONSTRUCT (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState006 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | CASE | DOUBLESEPARATOR | EOF | TYPE ->
          let c = _v in
          let _v = _menhir_action_78 c in
          _menhir_goto_typecase _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_007 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState008 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | CASE | DOUBLESEPARATOR | EOF | TYPE ->
          let t = _v in
          let _v = _menhir_action_84 t in
          _menhir_goto_typeproduct _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_typeproduct : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState006 ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState008 ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_010 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_CONSTRUCT -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_CONSTRUCT (_menhir_stack, _menhir_s, c) = _menhir_stack in
      let t = _v in
      let _v = _menhir_action_79 c t in
      _menhir_goto_typecase _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_typecase : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | CASE ->
          let _menhir_stack = MenhirCell1_typecase (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState014 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | CONSTRUCT _v ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | DOUBLESEPARATOR | EOF | TYPE ->
          let c = _v in
          let _v = _menhir_action_83 c in
          _menhir_goto_typelist _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_typelist : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState004 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState014 ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState011 ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_016 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, name) = _menhir_stack in
      let l = _v in
      let _v = _menhir_action_80 l name in
      _menhir_goto_typedef _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_typedef : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState002 ->
          _menhir_run_211 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState022 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState018 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_211 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_TYPE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let t = _v in
      let _v = _menhir_action_86 t in
      _menhir_goto_types _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_types : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_TYPE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TYPE ->
          let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState018 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | EOF ->
          let MenhirCell1_TYPE (_menhir_stack, _) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_53 t in
          _menhir_goto_main _menhir_stack _v
      | DOUBLESEPARATOR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_1 ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState021
          | TYPE ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              let _menhir_stack = MenhirCell1_TYPE (_menhir_stack, MenhirState021) in
              let _menhir_s = MenhirState022 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | TRY ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState021
          | REF ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState021
          | RAISE ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState021
          | PRINT ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState021
          | NOT ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState021
          | MINUS ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState021
          | MATCH ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState021
          | LPAREN ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState021
          | LET ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState021
          | LBRACKET ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState021
          | INT _v_3 ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState021
          | IF ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState021
          | FUNCTION ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState021
          | FUN ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState021
          | EXCEPT ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState021
          | EOF ->
              let MenhirCell1_TYPE (_menhir_stack, _) = _menhir_stack in
              let t = _v in
              let _v = _menhir_action_52 t in
              _menhir_goto_main _menhir_stack _v
          | CONSTRUCT _v_4 ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState021
          | BOOL _v_5 ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState021
          | BEGIN ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState021
          | BANG ->
              let _menhir_stack = MenhirCell1_types (_menhir_stack, _menhir_s, _v) in
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState021
          | _ ->
              _eRR ())
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_023 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_TYPE, _menhir_box_main) _menhir_cell1_types, _menhir_box_main) _menhir_cell1_TYPE -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_TYPE (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_types (_menhir_stack, _menhir_s, m) = _menhir_stack in
      let t = _v in
      let _v = _menhir_action_88 m t in
      _menhir_goto_types _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_019 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_TYPE, _menhir_box_main) _menhir_cell1_types -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_types (_menhir_stack, _menhir_s, m) = _menhir_stack in
      let t = _v in
      let _v = _menhir_action_87 m t in
      _menhir_goto_types _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_015 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_typecase -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_typecase (_menhir_stack, _menhir_s, c) = _menhir_stack in
      let l = _v in
      let _v = _menhir_action_82 c l in
      _menhir_goto_typelist _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_012 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_cell1_CASE -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_CASE (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, name) = _menhir_stack in
      let l = _v in
      let _v = _menhir_action_81 l name in
      _menhir_goto_typedef _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_009 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, t) = _menhir_stack in
      let l = _v in
      let _v = _menhir_action_85 l t in
      _menhir_goto_typeproduct _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState000 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TYPE ->
          let _menhir_stack = MenhirCell1_TYPE (_menhir_stack, _menhir_s) in
          let _menhir_s = MenhirState002 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | TRY ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RAISE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUNCTION ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXCEPT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTRUCT _v ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BEGIN ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
end

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

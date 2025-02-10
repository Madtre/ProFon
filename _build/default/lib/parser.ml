
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | VAR of (
# 19 "lib/parser.mly"
       (string)
# 15 "lib/parser.ml"
  )
    | TIMES
    | THEN
    | SEPARATOR
    | RPAREN
    | RIGHTARROW
    | REF
    | REC
    | PRINT
    | PLUS
    | OR
    | NOT
    | MINUS
    | LPAREN
    | LET
    | INT of (
# 17 "lib/parser.mly"
       (int)
# 34 "lib/parser.ml"
  )
    | IN
    | IF
    | FUN
    | EQUAL
    | EOL
    | ELSE
    | BOOL of (
# 18 "lib/parser.mly"
       (bool)
# 45 "lib/parser.ml"
  )
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
let rec aux (m : expr) (e : expr) : expr = match m with
|Fun(v, t) -> Fun(v, aux t e)
|Var v -> Fun(Var v, e)
|_ -> failwith "comportement innatendu de la grammaire"

# 68 "lib/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_main) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: main. *)

  | MenhirState02 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 02.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState03 : (('s, _menhir_box_main) _menhir_cell1_REF, _menhir_box_main) _menhir_state
    (** State 03.
        Stack shape : REF.
        Start symbol: main. *)

  | MenhirState04 : ((('s, _menhir_box_main) _menhir_cell1_REF, _menhir_box_main) _menhir_cell1_LPAREN, _menhir_box_main) _menhir_state
    (** State 04.
        Stack shape : REF LPAREN.
        Start symbol: main. *)

  | MenhirState09 : (('s, _menhir_box_main) _menhir_cell1_PRINT, _menhir_box_main) _menhir_state
    (** State 09.
        Stack shape : PRINT.
        Start symbol: main. *)

  | MenhirState10 : (('s, _menhir_box_main) _menhir_cell1_NOT, _menhir_box_main) _menhir_state
    (** State 10.
        Stack shape : NOT.
        Start symbol: main. *)

  | MenhirState11 : (('s, _menhir_box_main) _menhir_cell1_MINUS, _menhir_box_main) _menhir_state
    (** State 11.
        Stack shape : MINUS.
        Start symbol: main. *)

  | MenhirState12 : (('s, _menhir_box_main) _menhir_cell1_LPAREN, _menhir_box_main) _menhir_state
    (** State 12.
        Stack shape : LPAREN.
        Start symbol: main. *)

  | MenhirState14 : (('s, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 14.
        Stack shape : LET VAR.
        Start symbol: main. *)

  | MenhirState15 : (('s, _menhir_box_main) _menhir_cell1_VAR, _menhir_box_main) _menhir_state
    (** State 15.
        Stack shape : VAR.
        Start symbol: main. *)

  | MenhirState17 : ((('s, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_EQUAL, _menhir_box_main) _menhir_state
    (** State 17.
        Stack shape : LET VAR EQUAL.
        Start symbol: main. *)

  | MenhirState18 : (('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_state
    (** State 18.
        Stack shape : IF.
        Start symbol: main. *)

  | MenhirState19 : (('s, _menhir_box_main) _menhir_cell1_FUN, _menhir_box_main) _menhir_state
    (** State 19.
        Stack shape : FUN.
        Start symbol: main. *)

  | MenhirState21 : ((('s, _menhir_box_main) _menhir_cell1_FUN, _menhir_box_main) _menhir_cell1_multivariables, _menhir_box_main) _menhir_state
    (** State 21.
        Stack shape : FUN multivariables.
        Start symbol: main. *)

  | MenhirState26 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 26.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState28 : (('s, _menhir_box_main) _menhir_cell1_applic, _menhir_box_main) _menhir_state
    (** State 28.
        Stack shape : applic.
        Start symbol: main. *)

  | MenhirState30 : ((('s, _menhir_box_main) _menhir_cell1_applic, _menhir_box_main) _menhir_cell1_LPAREN, _menhir_box_main) _menhir_state
    (** State 30.
        Stack shape : applic LPAREN.
        Start symbol: main. *)

  | MenhirState32 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 32.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState34 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 34.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState36 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 36.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState38 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 38.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState40 : (('s, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 40.
        Stack shape : expression.
        Start symbol: main. *)

  | MenhirState46 : ((('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 46.
        Stack shape : IF expression.
        Start symbol: main. *)

  | MenhirState48 : (((('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 48.
        Stack shape : IF expression expression.
        Start symbol: main. *)

  | MenhirState51 : (((('s, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_EQUAL, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 51.
        Stack shape : LET VAR EQUAL expression.
        Start symbol: main. *)

  | MenhirState54 : ((('s, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_multivariables, _menhir_box_main) _menhir_state
    (** State 54.
        Stack shape : LET VAR multivariables.
        Start symbol: main. *)

  | MenhirState56 : (((('s, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_multivariables, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 56.
        Stack shape : LET VAR multivariables expression.
        Start symbol: main. *)

  | MenhirState59 : (('s, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_state
    (** State 59.
        Stack shape : LET VAR.
        Start symbol: main. *)

  | MenhirState61 : ((('s, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_multivariables, _menhir_box_main) _menhir_state
    (** State 61.
        Stack shape : LET VAR multivariables.
        Start symbol: main. *)

  | MenhirState63 : (((('s, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_multivariables, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_state
    (** State 63.
        Stack shape : LET VAR multivariables expression.
        Start symbol: main. *)


and ('s, 'r) _menhir_cell1_applic = 
  | MenhirCell1_applic of 's * ('s, 'r) _menhir_state * (Expr.expr)

and ('s, 'r) _menhir_cell1_expression = 
  | MenhirCell1_expression of 's * ('s, 'r) _menhir_state * (Expr.expr)

and ('s, 'r) _menhir_cell1_multivariables = 
  | MenhirCell1_multivariables of 's * ('s, 'r) _menhir_state * (Expr.expr)

and ('s, 'r) _menhir_cell1_EQUAL = 
  | MenhirCell1_EQUAL of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_FUN = 
  | MenhirCell1_FUN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LET = 
  | MenhirCell1_LET of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_MINUS = 
  | MenhirCell1_MINUS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_NOT = 
  | MenhirCell1_NOT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_PRINT = 
  | MenhirCell1_PRINT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_REF = 
  | MenhirCell1_REF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_VAR = 
  | MenhirCell1_VAR of 's * ('s, 'r) _menhir_state * (
# 19 "lib/parser.mly"
       (string)
# 262 "lib/parser.ml"
)

and 's _menhir_cell0_VAR = 
  | MenhirCell0_VAR of 's * (
# 19 "lib/parser.mly"
       (string)
# 269 "lib/parser.ml"
)

and _menhir_box_main = 
  | MenhirBox_main of (Expr.expr) [@@unboxed]

let _menhir_action_01 =
  fun a s ->
    (
# 80 "lib/parser.mly"
                   ( FunCall(a, s) )
# 280 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_02 =
  fun v ->
    (
# 81 "lib/parser.mly"
               ( Var v )
# 288 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_03 =
  fun v ->
    (
# 94 "lib/parser.mly"
                                           ( v )
# 296 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_04 =
  fun e ->
    (
# 96 "lib/parser.mly"
                                           ( e )
# 304 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_05 =
  fun e1 e2 ->
    (
# 97 "lib/parser.mly"
                                           ( Accumulation(e1, e2) )
# 312 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_06 =
  fun e1 e2 ->
    (
# 98 "lib/parser.mly"
                                           ( Add(e1,e2) )
# 320 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_07 =
  fun e1 e2 ->
    (
# 99 "lib/parser.mly"
                                           ( Mul(e1,e2) )
# 328 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_08 =
  fun e1 e2 ->
    (
# 100 "lib/parser.mly"
                                           ( Min(e1,e2) )
# 336 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_09 =
  fun e1 e2 ->
    (
# 101 "lib/parser.mly"
                                           ( And(e1,e2) )
# 344 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_10 =
  fun e1 e2 ->
    (
# 102 "lib/parser.mly"
                                           ( Or(e1,e2) )
# 352 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_11 =
  fun e ->
    (
# 103 "lib/parser.mly"
                                           ( Not(e) )
# 360 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_12 =
  fun e1 e2 e3 ->
    (
# 105 "lib/parser.mly"
                                                             ( IfThenElse(e1,e2,e3) )
# 368 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_13 =
  fun e1 e2 ->
    (
# 106 "lib/parser.mly"
                                                             ( IfThenElse(e1,e2,Unit))
# 376 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_14 =
  fun e1 e2 v ->
    (
# 107 "lib/parser.mly"
                                                      ( LetIn(Var(v), e1, e2) )
# 384 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_15 =
  fun e v ->
    (
# 110 "lib/parser.mly"
                                                                 ( aux v e)
# 392 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_16 =
  fun e v vs ->
    (
# 111 "lib/parser.mly"
                                                                 ( LetIn(Var v, aux vs e, Var v) )
# 400 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_17 =
  fun e v vs ->
    (
# 112 "lib/parser.mly"
                                                                     ( LetRecIn(Var v, aux vs e, Var v) )
# 408 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_18 =
  fun e e2 v vs ->
    (
# 113 "lib/parser.mly"
                                                                                  ( LetIn(Var v, aux vs e, e2) )
# 416 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_19 =
  fun e e2 v vs ->
    (
# 114 "lib/parser.mly"
                                                                                      ( LetRecIn(Var v, aux vs e, e2) )
# 424 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_20 =
  fun e ->
    (
# 116 "lib/parser.mly"
                                           ( Min(Cst 0, e) )
# 432 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_21 =
  fun e ->
    (
# 117 "lib/parser.mly"
                                           ( PrInt e )
# 440 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_22 =
  fun v ->
    (
# 118 "lib/parser.mly"
                                           ( Ref v )
# 448 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_23 =
  fun v ->
    (
# 119 "lib/parser.mly"
                                           ( Access(Var v) )
# 456 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_24 =
  fun e v ->
    (
# 120 "lib/parser.mly"
                                           ( Assign(Var v, e) )
# 464 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_25 =
  fun a ->
    (
# 122 "lib/parser.mly"
                                           ( a )
# 472 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_26 =
  fun e ->
    (
# 64 "lib/parser.mly"
                 ( e )
# 480 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_27 =
  fun m v ->
    (
# 89 "lib/parser.mly"
                             ( Fun(Var(v), m))
# 488 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_28 =
  fun v ->
    (
# 90 "lib/parser.mly"
          (Var(v))
# 496 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_29 =
  fun v ->
    (
# 84 "lib/parser.mly"
                             ( v )
# 504 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_30 =
  fun v ->
    (
# 85 "lib/parser.mly"
                             ( Var(v) )
# 512 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_31 =
  fun e ->
    (
# 86 "lib/parser.mly"
                             ( e )
# 520 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_32 =
  fun i ->
    (
# 70 "lib/parser.mly"
                                       ( Cst i)
# 528 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_33 =
  fun b ->
    (
# 75 "lib/parser.mly"
                                       ( Bool b )
# 536 "lib/parser.ml"
     : (Expr.expr))

let _menhir_action_34 =
  fun () ->
    (
# 76 "lib/parser.mly"
                                       ( Unit )
# 544 "lib/parser.ml"
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
    | BOOL _ ->
        "BOOL"
    | ELSE ->
        "ELSE"
    | EOL ->
        "EOL"
    | EQUAL ->
        "EQUAL"
    | FUN ->
        "FUN"
    | IF ->
        "IF"
    | IN ->
        "IN"
    | INT _ ->
        "INT"
    | LET ->
        "LET"
    | LPAREN ->
        "LPAREN"
    | MINUS ->
        "MINUS"
    | NOT ->
        "NOT"
    | OR ->
        "OR"
    | PLUS ->
        "PLUS"
    | PRINT ->
        "PRINT"
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
    | VAR _ ->
        "VAR"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ASSIGN ->
          let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState02 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REF ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL _v ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND | BOOL _ | ELSE | EOL | IN | INT _ | LPAREN | MINUS | OR | PLUS | RPAREN | SEPARATOR | THEN | TIMES | VAR _ ->
          let v = _v in
          let _v = _menhir_action_02 v in
          _menhir_goto_applic _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_REF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState03 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | RPAREN ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer
          | _ ->
              _eRR ())
      | INT _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_05 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_LPAREN -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_34 () in
      _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_value : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState28 ->
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState00 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState02 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState09 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState10 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState11 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState12 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState63 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState61 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState56 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState54 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState51 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState17 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState48 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState46 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState18 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState40 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState38 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState36 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState34 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState32 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState30 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState26 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState21 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_43 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_applic -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let v = _v in
      let _v = _menhir_action_29 v in
      _menhir_goto_sexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_sexpr : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_applic -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_applic (_menhir_stack, _menhir_s, a) = _menhir_stack in
      let s = _v in
      let _v = _menhir_action_01 a s in
      _menhir_goto_applic _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_applic : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let v = _v_0 in
          let _v = _menhir_action_30 v in
          _menhir_goto_sexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LPAREN ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, MenhirState28) in
          let _menhir_s = MenhirState30 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | RPAREN ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer
          | REF ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL _v ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | INT _v_5 ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState28
      | BOOL _v_6 ->
          let _menhir_stack = MenhirCell1_applic (_menhir_stack, _menhir_s, _v) in
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState28
      | AND | ELSE | EOL | IN | MINUS | OR | PLUS | RPAREN | SEPARATOR | THEN | TIMES ->
          let a = _v in
          let _v = _menhir_action_25 a in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_09 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PRINT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState09 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_10 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_NOT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState10 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_11 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState11 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_12 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState12 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | RPAREN ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer
      | REF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_13 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
          let _menhir_s = MenhirState14 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | EQUAL ->
              let _menhir_stack = MenhirCell1_EQUAL (_menhir_stack, _menhir_s) in
              let _menhir_s = MenhirState17 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | REF ->
                  _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PRINT ->
                  _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | NOT ->
                  _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MINUS ->
                  _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LET ->
                  _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT _v ->
                  _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IF ->
                  _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FUN ->
                  _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL _v ->
                  _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | REC ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
              let _menhir_s = MenhirState59 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_15 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState15
      | EQUAL | RIGHTARROW ->
          let v = _v in
          let _v = _menhir_action_28 v in
          _menhir_goto_multivariables _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_multivariables : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState59 ->
          _menhir_run_60 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState14 ->
          _menhir_run_53 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState19 ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState15 ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_60 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_multivariables (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQUAL ->
          let _menhir_s = MenhirState61 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REF ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL _v ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_06 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let i = _v in
      let _v = _menhir_action_32 i in
      _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_18 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState18 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_19 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FUN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState19 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_07 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let b = _v in
      let _v = _menhir_action_33 b in
      _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_22 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let v = _v in
          let _v = _menhir_action_23 v in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_72 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState02 ->
          _menhir_run_70 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState09 ->
          _menhir_run_69 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState10 ->
          _menhir_run_68 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState11 ->
          _menhir_run_67 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState12 ->
          _menhir_run_65 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState63 ->
          _menhir_run_64 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState61 ->
          _menhir_run_62 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState56 ->
          _menhir_run_57 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState54 ->
          _menhir_run_55 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState51 ->
          _menhir_run_52 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState17 ->
          _menhir_run_50 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState48 ->
          _menhir_run_49 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState46 ->
          _menhir_run_47 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState18 ->
          _menhir_run_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState40 ->
          _menhir_run_41 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState38 ->
          _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState36 ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState34 ->
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState32 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState30 ->
          _menhir_run_31 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState26 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState21 ->
          _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_72 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOL ->
          let e = _v in
          let _v = _menhir_action_26 e in
          MenhirBox_main _v
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_26 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState26 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_32 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState32 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_34 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState34 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_36 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState36 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_38 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState38 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_40 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState40 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_70 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE | EOL | IN | RPAREN | SEPARATOR | THEN ->
          let MenhirCell1_VAR (_menhir_stack, _menhir_s, v) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_24 e v in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_69 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_PRINT -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_PRINT (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_21 e in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_68 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_NOT -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_NOT (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_11 e in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_67 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_MINUS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ELSE | EOL | IN | MINUS | OR | PLUS | RPAREN | SEPARATOR | THEN ->
          let MenhirCell1_MINUS (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_20 e in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_65 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_04 e in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_64 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_multivariables, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expression (_menhir_stack, _, e) = _menhir_stack in
      let MenhirCell1_multivariables (_menhir_stack, _, vs) = _menhir_stack in
      let MenhirCell0_VAR (_menhir_stack, v) = _menhir_stack in
      let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_19 e e2 v vs in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_62 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_multivariables as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IN ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState63 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REF ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL _v ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE | EOL | RPAREN | SEPARATOR | THEN ->
          let MenhirCell1_multivariables (_menhir_stack, _, vs) = _menhir_stack in
          let MenhirCell0_VAR (_menhir_stack, v) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_17 e v vs in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_57 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_multivariables, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expression (_menhir_stack, _, e) = _menhir_stack in
      let MenhirCell1_multivariables (_menhir_stack, _, vs) = _menhir_stack in
      let MenhirCell0_VAR (_menhir_stack, v) = _menhir_stack in
      let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_18 e e2 v vs in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_55 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_multivariables as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IN ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState56 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REF ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL _v ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE | EOL | RPAREN | SEPARATOR | THEN ->
          let MenhirCell1_multivariables (_menhir_stack, _, vs) = _menhir_stack in
          let MenhirCell0_VAR (_menhir_stack, v) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_16 e v vs in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_52 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_EQUAL, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expression (_menhir_stack, _, e1) = _menhir_stack in
      let MenhirCell1_EQUAL (_menhir_stack, _) = _menhir_stack in
      let MenhirCell0_VAR (_menhir_stack, v) = _menhir_stack in
      let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_14 e1 e2 v in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_50 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_main) _menhir_cell1_EQUAL as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEPARATOR ->
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IN ->
          let _menhir_s = MenhirState51 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REF ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL _v ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND ->
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_49 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expression (_menhir_stack, _, e2) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, e1) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
      let e3 = _v in
      let _v = _menhir_action_12 e1 e2 e3 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_47 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState48 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REF ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL _v ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND | EOL | IN | MINUS | OR | PLUS | RPAREN | SEPARATOR | THEN | TIMES ->
          let MenhirCell1_expression (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_13 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_45 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | THEN ->
          let _menhir_s = MenhirState46 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REF ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL _v ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | SEPARATOR ->
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_41 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ELSE | EOL | IN | MINUS | OR | PLUS | RPAREN | SEPARATOR | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_09 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_39 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ELSE | EOL | IN | MINUS | OR | PLUS | RPAREN | SEPARATOR | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_08 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_37 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ELSE | EOL | IN | MINUS | OR | PLUS | RPAREN | SEPARATOR | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_10 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_35 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ELSE | EOL | IN | MINUS | OR | PLUS | RPAREN | SEPARATOR | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_06 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_33 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE | EOL | IN | RPAREN | SEPARATOR | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_05 e1 e2 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_31 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_applic, _menhir_box_main) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEPARATOR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_31 e in
          _menhir_goto_sexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_27 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expression (_menhir_stack, _menhir_s, e1) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_07 e1 e2 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_25 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_FUN, _menhir_box_main) _menhir_cell1_multivariables as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE | EOL | IN | RPAREN | SEPARATOR | THEN ->
          let MenhirCell1_multivariables (_menhir_stack, _, v) = _menhir_stack in
          let MenhirCell1_FUN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_15 e v in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_53 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_multivariables (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQUAL ->
          let _menhir_s = MenhirState54 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REF ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL _v ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_20 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_FUN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_multivariables (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RIGHTARROW ->
          let _menhir_s = MenhirState21 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REF ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRINT ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FUN ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL _v ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_16 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_VAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, v) = _menhir_stack in
      let m = _v in
      let _v = _menhir_action_27 m v in
      _menhir_goto_multivariables _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_24 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let v = _v in
      let _v = _menhir_action_03 v in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_08 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_REF -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_REF (_menhir_stack, _menhir_s) = _menhir_stack in
      let v = _v in
      let _v = _menhir_action_22 v in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState00 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRINT ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FUN ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
end

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

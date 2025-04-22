
(* The type of tokens. *)

type token = 
  | WITH
  | VAR of (string)
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
  | INT of (int)
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
  | CONSTRUCT of (string)
  | COMMA
  | CASE
  | BOOL of (bool)
  | BEGIN
  | BANG
  | ASSIGN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Expr.expr)

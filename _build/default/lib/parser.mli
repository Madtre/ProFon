
(* The type of tokens. *)

type token = 
  | WITH
  | WHILE
  | VAR of (string)
  | UNDERPLUS
  | TRY
  | TO
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
  | FOR
  | EXCEPT
  | EQUAL
  | EOF
  | END
  | ELSE
  | DOUBLESEPARATOR
  | DONE
  | DO
  | DIV
  | DIFF
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

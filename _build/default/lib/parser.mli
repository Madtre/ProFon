
(* The type of tokens. *)

type token = 
  | VAR of (string)
  | TIMES
  | THEN
  | RPAREN
  | PRINT
  | PLUS
  | OR
  | NOT
  | MINUS
  | LPAREN
  | LET
  | INT of (int)
  | IN
  | IF
  | EQUAL
  | EOL
  | ELSE
  | BOOL of (bool)
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Expr.expr)

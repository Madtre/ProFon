
(* The type of tokens. *)

type token = 
  | TIMES
  | THEN
  | RPAREN
  | PLUS
  | OR
  | NOT
  | MINUS
  | LPAREN
  | INT of (int)
  | IF
  | EOL
  | ELSE
  | BOOL of (bool)
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Expr.expr)

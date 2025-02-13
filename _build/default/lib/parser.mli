
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR of (string)
  | TO
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
  | INT of (int)
  | IN
  | IF
  | FUN
  | FOR
  | EQUAL
  | EOL
  | ELSE
  | DONE
  | DO
  | COMMA
  | BOOL of (bool)
  | BANG
  | ASSIGN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Expr.expr)

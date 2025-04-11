{
  (* prélude du fichier *)
  open Parser
          
}

(* définitions d'expressions régulières *)
let chiffre = ['0'-'9']
let nombre = chiffre+
let boolean = "true" | "false"
let var = ['a'-'z']+['a'-'z' 'A'-'Z' '0'-'9']*|"()"|'_'

(* définition des tokens *)
               
rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n']     { token lexbuf }    (* on saute les blancs et les tabulations *)
 	     	   	           (* en faisant cet appel récursif à "token" *)
  | eof           { EOF }   (*EndOfLine ; à noter que la fin de fichier se note "eof" *)
  | '+'             { PLUS }
  | '*'             { TIMES }
  | '-'             { MINUS }
  | '/'             { DIV }
  | '('             { LPAREN } 
  | ')'             { RPAREN }
  | "begin"         { BEGIN }
  | "end"           { END }
  | "<="            { LEQ }
  | "<"             { LT }
  | ">="            { GEQ }
  | ">"             { GT }
  | "<>"            { DIFF }
  | "&&"            { AND }
  | "||"            { OR }
  | "not"           { NOT }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "prInt"         { PRINT }
  | "let"           { LET }
  | "in"            { IN }
  | '='             { EQUAL }
  | "fun"           { FUN }
  | "function"      { FUNCTION }
  | "->"            { RIGHTARROW }
  | "rec"           { REC }
  | "ref"           { REF }
  | "!"             { BANG }
  | ":="            { ASSIGN }
  | ";"             { SEPARATOR }
  | ";;"            { DOUBLESEPARATOR }
  | ","             { COMMA }
  | "for"           { FOR }
  | "while"         { WHILE }
  | "to"            { TO }
  | "do"            { DO }
  | "done"          { DONE }
  | "["             { LBRACKET }
  | "]"             { RBRACKET }
  | "::"            { QUATROSPUNTOS }
  | "match"         { MATCH }
  | "with"          { WITH }
  | "|"             { CASE }
  | "try"           { TRY }
  | "E"             { EXCEPT }
  | "raise"         { RAISE }

  | nombre as s { INT (int_of_string s) }
  | boolean as b { BOOL (bool_of_string b) }
  | var as v { VAR v }

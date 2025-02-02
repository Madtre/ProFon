{
  (* prélude du fichier *)
  open Parser
          
}

(* définitions d'expressions régulières *)
let chiffre = ['0'-'9']
let nombre = chiffre+
let boolean = "true" | "false"

               
rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']     { token lexbuf }    (* on saute les blancs et les tabulations *)
 	     	   	           (* en faisant cet appel récursif à "token" *)
  | '\n'            { EOL }   (*EndOfLine ; à noter que la fin de fichier se note "eof" *)
  | '+'             { PLUS }
  | '*'             { TIMES }
  | '-'             { MINUS }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | "and"           { AND }
  | "or"            { OR }
  | "not"           { NOT }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | nombre as s { INT (int_of_string s) }
  | boolean as s { BOOL (bool_of_string s) }

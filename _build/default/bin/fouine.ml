open Lib
open Expr


(* le traitement d'une expression en entrée *)   
let execute e =
  begin
    affiche_expr e; (* on affiche e *)
    print_newline();
    let v =  eval e in (* on évalue e *)
    affiche_valeur v;
    print_newline();
  end

(* "incantations" qu'il n'est pas nécessaire de comprendre dans un premier
   temps : on récupère l'entrée, dans un fichier ou sur le clavier *)
let nom_fichier = ref ""

let recupere_entree () =

  Arg.parse (* ci-dessous les 3 arguments de Arg.parse : *)
    [] (* la liste des options, vide *)
    (fun s -> nom_fichier := s) (* la fonction a declencher lorsqu'on recupere un string qui n'est pas une option : ici c'est le nom du fichier, et on stocke cette information dans la reference nom_fichier *)
    ""; (* le message d'accueil, qui est vide *)
  try
    let where_from = match !nom_fichier with
      | "" -> stdin
      | s -> open_in s in
    let lexbuf = Lexing.from_channel where_from in
    let parse () = Parser.main Lexer.token lexbuf in
    parse () 
  with e -> (Printf.printf "problème de saisie\n"; raise e)



(* la fonction principale *)
let run () =
  try
      let saisie = recupere_entree () in
	execute saisie; flush stdout
  with e -> raise e  (* <-- en cas d'exception *)


let _ = run ()


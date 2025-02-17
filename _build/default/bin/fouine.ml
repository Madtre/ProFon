open Lib
open Expr
open Interprete


(* "incantations" qu'il n'est pas n�cessaire de comprendre dans un premier
   temps : on r�cup�re l'entr�e, dans un fichier ou sur le clavier *)
let nom_fichier = ref ""
let recupere_entree () =
  let optlist = [
    ("-debug", Arg.Set debug_mode, "Active le mode de debuggage" );
    ("-show-src", Arg.Set src_mode, "Ecrit les operateurs en majuscules")
  ] in

  let usage = "Bienvenue a bord." in  (* message d'accueil, option -help *)

  Arg.parse (* ci-dessous les 3 arguments de Arg.parse : *)
    optlist (* la liste des options definie plus haut *)

    (fun s -> nom_fichier := s) (* la fonction a declencher lorsqu'on recupere un string qui n'est pas une option : ici c'est le nom du fichier, et on stocke cette information dans la reference nom_fichier *)
    usage; (* le message d'accueil *)
  try
    let where_from = match !nom_fichier with
      | "" -> stdin
      | s -> open_in s in
    let lexbuf = Lexing.from_channel where_from in
    let parse () = Parser.main Lexer.token lexbuf in
    parse () 
  with e -> (Printf.printf "problème de saisie\n"; raise e)


(* le traitement d'une expression en entr�e *)   
let execute e =
  begin
    let _ =  eval e in ()
  end

(* la fonction principale *)
let run () =
  try
      let saisie = recupere_entree () in
	execute saisie; flush stdout
  with e -> raise e  (* <-- en cas d'exception *)


let _ = run ()
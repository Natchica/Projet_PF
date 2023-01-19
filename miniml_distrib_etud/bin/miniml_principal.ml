(* ouverture de la "library" definie dans lib/dune *)
open Miniml

(* ouverture de modules de la library Miniml *)
open Miniml_lexer
open Miniml_parser
open Miniml_typer

(* ******** à compléter ********* *)

(* main : unit -> unit *)
(* Analyse le contenu d'un fichier passé en paramètre ou l'entrée standard si aucun fichier n'est donné *)
(* Affiche OK si l'analyse syntaxique c'est bien passée et KO sinon *)
let main () =
    let flux = read_miniml_tokens_from_file Sys.argv.(1) in
    match (parseE flux)
     with
       | (true, _) -> print_endline "Ok"
       | _ -> print_endline "Ko"
;;

main();;
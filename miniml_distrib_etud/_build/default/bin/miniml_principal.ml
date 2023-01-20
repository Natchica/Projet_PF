(* ouverture de la "library" definie dans lib/dune *)
open Miniml

(* ouverture de modules de la library Miniml *)
open Miniml_lexer
open Miniml_parser
open Miniml_typer
open Lazyflux

(* ******** à compléter ********* *)

(* main : unit -> unit *)
(* Analyse le contenu d'un fichier passé en paramètre ou l'entrée standard si aucun fichier n'est donné *)
(* Affiche OK si l'analyse syntaxique c'est bien passée et KO sinon *)
let main () =
    let flux = read_miniml_tokens_from_file Sys.argv.(1) in


    let rec loop f = match Flux.uncons f with
      | None -> print_endline "End of file"
      | Some _ -> 
        let (b, next) = parseE f in 
        if b then (print_endline "Line ok"; loop next) 
        else print_endline "Ko"
    in loop flux

   


;;

main();;
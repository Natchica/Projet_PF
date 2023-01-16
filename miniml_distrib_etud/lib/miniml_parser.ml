open Miniml_types
open Miniml_lexer
open Lazyflux

(* Fonction de lecture d'un fichier.    *)
(* Produit le flux des lexèmes reconnus *)
let read_miniml_tokens_from_file filename : token Flux.t =
  try
    let chan = open_in filename in
    let buf = Lexing.from_channel chan in
    line_g := 1;
    let next_token () =
      try
        let next = token buf in
        if next = EOF
        then
          begin
            close_in chan;
            None
          end
        else
          Some (next, ())
   with
   | ErreurLex msg ->
      begin
        close_in chan;
        raise (ErreurLecture (Format.sprintf "ERREUR : ligne %d, lexème '%s' : %s" !line_g (Lexing.lexeme buf) msg))
      end in
    Flux.unfold next_token ()
 with
    | Sys_error _ -> raise (ErreurLecture (Format.sprintf "ERREUR : Impossible d'ouvrir le fichier '%s' !" filename))
;;

(* Fonction de lecture d'un buffer.   *)
(* Similaire à la fonction précédente *)
let read_miniml_tokens_from_lexbuf buf : token Flux.t =
  line_g := 1;
  let next_token () =
    try
      let next = token buf in
      if next = EOF
      then
        begin
          None
        end
      else
        Some (next, ())
    with
    | ErreurLex msg ->
       begin
         raise (ErreurLecture (Format.sprintf "ERREUR : ligne %d, lexème '%s' : %s" !line_g (Lexing.lexeme buf) msg))
       end in
  Flux.unfold next_token ()
;;

(* Fonction de lecture d'une chaîne.  *)
(* Similaire à la fonction précédente *)
let read_miniml_tokens_from_string chaine : token Flux.t =
  read_miniml_tokens_from_lexbuf (Lexing.from_string chaine)
;;

(* Fonctions auxiliaires de traitement des lexèmes *)
(* contenant une information: IDENT, BOOL et INT   *)
let isident =
  function IDENT _     -> true
         | _           -> false
let isbool =
  function BOOL _      -> true
         | _           -> false
let isint =
  function INT _       -> true
         | _           -> false

let unident =
  function
  | IDENT id    -> id
  | _           -> assert false
let unbool =
  function
  | BOOL b      -> b
  | _           -> assert false   
let unint =
  function
  | INT i       -> i
  | _           -> assert false


(* Fonctions de parsing de MiniML *)
(* ******** à compléter ********* *)

let inject : token Flux.t -> (bool, token Flux.t) = function flux ->
  (true, flux)

let accept : token -> token Flux.t -> (bool, token Flux.t) = function expected flux ->
  let (a, next) = pickAndAdvance flux in 
    match a with
    | a when a = expected -> (true, next)
    | _ -> (false, next)

let acceptIdent : token Flux.t -> (bool, token Flux.t) = function flux ->
  let (a, next) = pickAndAdvance flux in (isident a, next)

let acceptInt : token Flux.t -> (bool, token Flux.t) = function flux ->
  let (a, next) = pickAndAdvance flux in (isint a, next)

let acceptBool : token Flux.t -> (bool, token Flux.t) = function flux ->
  let (a, next) = pickAndAdvance flux in (isbool a, next)

let (>>=) result f =
  match result with
  | (true, next) -> f next
  | (false, _) -> false

let pickAndAdvance : token Flux.t -> (token -> token Flux.t) = function tokens ->
  match uncons tokens with
  | None -> failwith "Le flux est vide"
  | Some(a, next) -> (a, next)

(* parseE : parseur d'une expression *)
(* parseL : parseur d'une liaison *)
(* parseBi : parseur d'une opération binaire *)
(* parseA : parseur d'une opération arithmetique *)
(* parseBo : parseur d'une opération booleene *)
(* ParseR : parseur d'une opération relationnelle*)
(* parseC : parseur d'une constante *)

(*  E -> let X               *)
(*    -> ( Y                 *)
(*    -> if E then E else E  *)
(*    -> ident               *)
(*    -> Constant            *)
let rec parserE : token Flux.t -> bool = function flux ->
  (print_string "Expr -> ");
  let (a, next) = pickAndAdvance flux in
  match a with
  | LET -> inject flux >>= accept LET >>= parseX
  | PARO -> inject flux >>= accept PARO >>= parseY
  | IF -> inject flux >>= accept IF >>= parseE >>= accept THEN >>= parse E >>= accept ELSE >>= parseE
  | IDENT _ -> inject flux >>= acceptIdent
  | _ -> parseC

(*  X -> L in E      *)
(*    -> rec L in E  *)
let rec parseX : token Flux.t -> bool = function flux ->
  (* TODO *)

(*  Y -> E Z             *)
(*    -> fun ident -> E  *)
let rec parseY : token Flux.t -> bool = function flux ->
  (* TODO *)

(*  Z -> B E )  *)
(*    -> )      *)
(*    -> E )    *)
let rec parseZ : token Flux.t -> bool = function flux ->
  (* TODO *)

(*  L -> ident = E  *)
let rec parseL : token Flux.t -> bool = function flux ->
  (* TODO *)

(*  Bi -> A | Bo | R | @ | ::  *)
let rec parseBi : token Flux.t -> bool = function flux ->
  (* TODO *)

(*  A -> + | - | * | /  *)
let rec parseA : token Flux.t -> bool = function flux ->
  (* TODO *)

(*  Bo -> && | ||  *)
let rec parseBo : token Flux.t -> bool = function flux ->
  (* TODO *)

(*  R -> = | <> | <= | < | >= | >  *)
let rec parseR : token Flux.t -> bool = function flux ->
  (* TODO *)

(*  C -> entier | booleen | [] | ()  *)
let rec parseC : token Flux.t -> bool = function flux ->
  (* TODO *)
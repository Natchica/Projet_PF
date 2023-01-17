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
let rec parseE : token Flux.t -> bool = function flux ->
  (print_string "Expr -> ");
  let (a, next) = pickAndAdvance flux in
  match a with
  | LET -> inject flux >>= accept LET >>= parseX
  | PARO -> inject flux >>= accept PARO >>= parseY
  | IF -> inject flux >>= accept IF >>= parseE >>= accept THEN >>= parseE >>= accept ELSE >>= parseE
  | IDENT _ -> inject flux >>= acceptIdent
  | _ -> parseC

(*  X -> L in E      *)
(*    -> rec L in E  *)
let rec parseX : token Flux.t -> bool = function flux ->
  (print_string "let -> ");
  let (a, next) = pickAndAdvance flux in
  match a with
  | IDENT _ -> inject flux >>= parseL >>= accept IN >>= parseE
  | REC -> inject flux >>= accept REC >>= parseL >>= accept IN >>= parseE
  | _ -> failwith "erreur parseX"

(*  Y -> E Z             *)
(*    -> fun ident -> E  *)
let rec parseY : token Flux.t -> bool = function flux ->
  (print_string "( -> ");
  let (a, next) = pickAndAdvance flux in
  match a with
  | FUN -> inject flux >>= accept FUN >>= acceptIdent >>= accept TO >>= parseE
  | _ -> inject flux >>= parseE >>= parseZ

(*  Z -> B E )  *)
(*    -> )      *)
(*    -> E )    *)
let rec parseZ : token Flux.t -> bool = function flux ->
  (print_string "( Expr -> ");
  let (a, next) = pickAndAdvance flux in
  match a with
  | PARF -> inject flux >>= accept PARF
  | PLUS | MOINS | MULT | DIV | AND | OR | EQU | NOTEQ | INFEQ | INF | SUPEQ | SUP | INT _ | BOOL _ | CROO | PARO -> inject flux >>= parseBi >>= parseE >>= accept PARF
  | _ -> inject flux >>= parseE >>= accept PARF

(*  L -> ident = E  *)
let rec parseL : token Flux.t -> bool = function flux ->
  (print_string "Liaison -> ");
  inject flux >>= acceptIdent >>= accept EQU >>= parseE

(*  Bi -> A | Bo | R | @ | ::  *)
let rec parseBi : token Flux.t -> bool = function flux ->
  (print_string "Binop -> ");
  let (a, next) = pickAndAdvance flux in
  match a with
  | PLUS | MOINS | MULT | DIV -> inject flux >>= parseA
  | AND | OR -> inject flux >>= parseBo
  | EQU | NOTEQ | INFEQ | INF | SUPEQ | SUP -> inject flux >>= parseR
  | INT _ | BOOL _ | CROO | PARO -> inject flux >>= parseC
  | _ -> failwith "erreur parseBi"

(*  A -> + | - | * | /  *)
let rec parseA : token Flux.t -> bool = function flux ->
  (print_string "Arithop -> ");
  let (a, next) = pickAndAdvance flux in
  match a with
  | PLUS -> inject flux >>= accept PLUS
  | MOINS -> inject flux >>= accept MOINS
  | MULT -> inject flux >>= accept MULT
  | DIV -> inject flux >>= accept DIV
  | _ -> failwith "erreur parseA"

(*  Bo -> && | ||  *)
let rec parseBo : token Flux.t -> bool = function flux ->
  (print_string "Boolop -> ");
  let (a, next) = pickAndAdvance flux in
  match a with
  | AND -> inject flux >>= accept AND
  | OR -> inject flux >>= accept OR
  | _ -> failwith "erreur parseBo"

(*  R -> = | <> | <= | < | >= | >  *)
let rec parseR : token Flux.t -> bool = function flux ->
  (print_string "Relop -> ");
  let (a, next) = pickAndAdvance flux in
  match a with
  | EQU -> inject flux >>= accept EQU
  | NOTEQ -> inject flux >>= accept NOTEQ
  | INFEQ -> inject flux >>= accept INFEQ
  | INF -> inject flux >>= accept INF
  | SUPEQ -> inject flux >>= accept SUPEQ
  | SUP -> inject flux >>= accept SUP
  | _ -> failwith "erreur parseR"

(*  C -> entier | booleen | [] | ()  *)
let rec parseC : token Flux.t -> bool = function flux ->
  (print_string "Constant -> ");
  let (a, next) = pickAndAdvance flux in
  match a with
  | INT _ -> inject flux >>= acceptIdent
  | BOOL _ -> inject flux >>= acceptBool
  | CROO -> inject flux >>= accept CROO >>= accept CROF
  | PARO -> inject flux >>= accept PARO >>= accept PARF
  | _ -> failwith "erreur parseC"
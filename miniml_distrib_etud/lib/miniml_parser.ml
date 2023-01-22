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

let pickAndAdvance : token Flux.t -> (token * token Flux.t) = fun tokens ->
  match Flux.uncons tokens with
  | None -> failwith "Le flux est vide"
  | Some(a, next) -> (a, next)

let accept : token -> token Flux.t -> token Flux.t = fun expected flux ->
  let (a, next) = pickAndAdvance flux in
    if a = expected then next else failwith "Parsing Error" 

let acceptIdent : token Flux.t -> (ident * token Flux.t) = function flux ->
  let (a, next) = pickAndAdvance flux in
    if isident a then (unident a, next) else failwith "Parsing Error"

let acceptInt : token Flux.t -> token Flux.t = function flux ->
  let (a, next) = pickAndAdvance flux in
    if isint a then next else failwith "Parsing Error"

let acceptBool : token Flux.t -> token Flux.t = function flux ->
  let (a, next) = pickAndAdvance flux in
    if isbool a then next else failwith "Parsing Error"

let (>>=) flux f = f flux


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
let rec parseE : token Flux.t -> expr * token Flux.t = function flux ->
  let (a, next) = pickAndAdvance flux in
  match a with
  | LET -> parseLet next
  | PARO -> parseY next
  | IF -> let (exp1, next1) = (parseE next) in let (exp2, next2) = parseE (next1 >>= accept THEN) in 
    let (exp3, next3) = parseE (next2 >>= accept ELSE) in (EIf(exp1, exp2, exp3), next3)
  | IDENT i -> (EIdent(i), next)
  | _ -> parseC flux

(*  X -> L in E      *)
(*    -> rec L in E  *)
and parseLet : token Flux.t -> (expr * token Flux.t) = function flux ->
  let (a, next) = pickAndAdvance flux in
  match a with
  | IDENT i -> let (exp1, next1) = parseE (next >>= accept EQU) in let (exp2, next2) = parseE (next1 >>= accept IN) 
    in (ELet(i, exp1, exp2), next2) 
  | REC -> let (i, next1) = (next >>= acceptIdent) in let (exp1, next2) = parseE (next1 >>= accept EQU) 
    in let (exp2, next3) = parseE (next2 >>= accept IN) in (ELetrec(i, exp1, exp2), next3) 
  | _ -> failwith "Parsing Error"

(*  Y -> E Z             *)
(*    -> fun ident -> E )  *)
and parseY : token Flux.t -> (expr * token Flux.t) = function flux ->
  let (a, next) = pickAndAdvance flux in
  match a with
  | FUN -> let (i, next1) = (next >>= acceptIdent) in let (exp, next1) = parseE (next1 >>= accept TO) in (EFun(i, exp), next1)
  | _ -> parseZ flux

(*  Z -> B E )  *)
(*    -> )      *)
(*    -> E )    *)
and parseZ : token Flux.t -> (expr * token Flux.t) = function flux ->
  let (exp1, next) = parseE flux in let (a, next1) = pickAndAdvance next in 
  match a with
  | PARF -> (exp1, next)
  | PLUS | MOINS | MULT | DIV | AND | OR | EQU | NOTEQ | INFEQ | INF | SUPEQ | SUP | CONCAT | CONS -> let b = EBinop a in let (_, next2) = parseE next1 in (b, next2) 
  | _ -> let (exp2, next2) = parseE next in (EApply(exp1, exp2), next2)

(*  C -> entier | booleen | [] | ()  *)
and parseC : token Flux.t -> (expr * token Flux.t) = function flux ->
  let (a, next) = pickAndAdvance flux in
  match a with
  | INT i -> (EConstant(CEntier(i)), next)
  | BOOL b -> (EConstant(CBooleen(b)), next)
  | CROO -> let next1 = next >>= accept CROF in (EConstant(CNil), next1)
  | PARO -> let next1 = next >>= accept PARF in (EConstant(CUnit), next1)
  | _ -> failwith "Parsing Error"
open Miniml_types
open Miniml_parser

(* signature minimale pour définir des variables *)
module type VariableSpec =
  sig
    (* type abstrait des variables      *)
    type t

    (* création d'une variable fraîche  *)
    val fraiche : unit -> t

    (* fonctions de comparaison         *)
    (* permet de définir des conteneurs *)
    (* (hash-table, etc) de variables   *)
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int

    (* fonction d'affichage             *)
    (* on utilise Format.std_formatter  *)
    (* comme premier paramètre          *)
    (* pour la sortie standard          *) 
    val fprintf : Format.formatter -> t -> unit
  end

(* implantation de la spécification     *)
module TypeVariable : VariableSpec (* with type t = int *)=
  struct
    type t = int

    let fraiche =
      let cpt = ref 0 in
      (fun () -> incr cpt; !cpt)

    let compare a b = a - b
    let equal a b = a = b
    let hash a = Hashtbl.hash a

    let fprintf fmt a = Format.fprintf fmt "t{%d}" a
  end


(* ******** à compléter ********* *)

(* create an empty environment *)
let gamma0 : (ident * 'a typ) list = []

(* add predefined types to the environment *)

let concat = TFun(TProd(TList(TVar()), TList(TVar())), TList(TVar()))
and cons = TFun(TProd(TVar(), TList(TVar())), TList(TVar()))
and pair = TFun(TProd(TVar(), TVar()), TProd(TVar(), TVar()))
and arithop = TFun(TProd(TInt, TInt), TInt)
and relop = TFun(TProd(TVar(), TVar()), TBool)
and boolop = TFun(TProd(TBool, TBool), TBool)
and not =  TFun(TBool, TBool)
and fst = TFun(TProd(TVar(), TVar()), TVar())
and snd = TFun(TProd(TVar(), TVar()), TVar())
and hd = TFun(TList(TVar()), TVar())

and tl = TFun(TList(TVar()), TList(TVar()))

let gamma0 = [("@",concat), ("::",cons), (",",pair), 
("Arithop",arithop), ("Relop",relop), 
("Boolop",boolop), ("not",not), ("fst",fst), 
("snd",snd), ("hd",hd), ("tl",tl)]

(* add a new type to the environment *)
let add (x, t) gamma = (x, t) :: gamma

(* find the type of a variable in the environment *)
let rec find x = function
  | [] -> raise Not_found
  | (y, t) :: gamma -> if y = x then t else find x gamma


let rec unify t1 t2 = match t1, t2 with
  | TVar a, TVar b -> if a = b then () else failwith "Erreur de type"
  | TVar a, _ -> if occurs a t2 then failwith "Erreur de type" else ()
  | _, TVar a -> if occurs a t1 then failwith "Erreur de type" else ()
  | TProd (a,b), TProd (c,d) -> unify a c; unify b d
  | TList a, TList b -> unify a b
  | TFun (a,b), TFun (c,d) -> unify a c; unify b d
  | TInt, TInt -> ()
  | TBool, TBool -> ()
  | TUnit, TUnit -> ()
  | _, _ -> failwith "Erreur de type"

and occurs a t = match t with
  | TVar b -> if a = b then true else false
  | TProd (a,b) -> if occurs a t || occurs b t then true else false
  | TList a -> if occurs a t then true else false
  | TFun (a,b) -> if occurs a t || occurs b t then true else false
  | TInt -> false
  | TBool -> false
  | TUnit -> false

let rec infer gamma = function
  | EConstant c -> (match c with
    | CEntier _ -> TInt
    | CBooleen _ -> TBool
    | CNil -> TList(TVar(TypeVariable.fraiche ()))
    | CUnit -> TUnit)

  | EIdent i -> find i gamma
  | EProd (e1,e2) ->  let a = infer gamma e1 and b = infer gamma e2 in TProd(a, b)
  | ECons (e1,e2) -> let a = infer gamma e1 and b = infer gamma e2 in if b = TList(a) then b else failwith "Erreur de type"
  | EFun (i,e) -> let a = TVar(TypeVariable.fraiche ()) in let gamma2 = (i,a)::gamma in let t = infer gamma2 e in TFun(a,t)
  | EIf (e1,e2,e3) -> let t1 = infer gamma e1 in unify t1 TBool; let t2 = infer gamma e2 in let t3 = infer gamma e3 in unify t2 t3; t2
  | EApply (e1,e2) ->  let a = TVar(TypeVariable.fraiche ()) in let t1 = infer gamma e1 in let t2 = infer gamma e2 in unify t1 (TFun(t2,a)); a
  | EBinop tok -> (match tok with
    | PLUS | MOINS | MULT | DIV | MOD -> TInt
    | AND | OR | INF | SUP | INFEQ | SUPEQ | EQU | NOTEQ -> TBool
    | _ -> TVar(TypeVariable.fraiche ())
    )

  | ELet (i,e1,e2) -> let t1 = infer gamma e1 in let gamma2 = (i,t1)::gamma in infer gamma2 e2
  | ELetrec (i,e1,e2) -> let a = TVar(TypeVariable.fraiche ()) in let gamma2 = (i,a)::gamma in let t1 = infer gamma2 e1 in unify a t1; infer gamma2 e2

  
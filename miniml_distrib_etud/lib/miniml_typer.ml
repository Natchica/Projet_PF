open Miniml_types

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
module VarHashtbl = Hashtbl.Make(TypeVariable)
let gamma0 = VarHashtbl.create 11
let arithm = (CONCAT , TFun(TProd(TInt, TInt), TInt))
let relop : token * 'a typ = (CONCAT, TFun(TProd(TVar(a), TVar(a)), TInt))
let i = TypeVariable.fraiche() ;;
VarHashtbl.add gamma0 i concat
let x = Hashtbl.find_all gamma0 1


(* create an empty environment *)
let gamma0 : (TypeVariable.t * 'a typ) list = []

(* add predefined types to the environment *)
let gamma0 = (TypeVariable.fraiche(), TList(TVar('a))) :: gamma0
let gamma0 = (TypeVariable.fraiche(), TFun(TProd(TVar('a), TVar('a)), TList(TVar('a)))) :: gamma0
let gamma0 = (TypeVariable.fraiche(), TFun(TProd(TVar('a), TVar('b)), TProd(TVar('a), TVar('b)))) :: gamma0
let gamma0 = (TypeVariable.fraiche(), TFun(TInt, TInt)) :: gamma0
let gamma0 = (TypeVariable.fraiche(), TFun(TVar('a), TBool)) :: gamma0
let gamma0 = (TypeVariable.fraiche(), TFun(TBool, TBool)) :: gamma0
let gamma0 = (TypeVariable.fraiche(), TFun(TVar('a), TVar('a))) :: gamma0
let gamma0 = (TypeVariable.fraiche(), TFun(TProd(TVar('a), TVar('b)), TVar('a))) :: gamma0
let gamma0 = (TypeVariable.fraiche(), TFun(TProd(TVar('a), TVar('b)), TVar('b))) :: gamma0
let gamma0 = (TypeVariable.fraiche(), TFun(TList(TVar('a)), TVar('a))) :: gamma0
let gamma0 = (TypeVariable.fraiche(), TFun(TList(TVar('a)), TList(TVar('a)))) :: gamma0

(* add some more types to the environment *)
let gamma0 = (TypeVariable.fraiche(), TFun(TVar('a), TUnit)) :: gamma0
let gamma0 = (TypeVariable.fraiche(), TFun(TProd(TVar('a), TVar('b)), TBool)) :: gamma0

(* use the environment in a type inference algorithm )
let infer_type (e: 'a expr) (Γ: (TypeVariable.t * 'a typ) list) : 'a typ =
( implementation of the type inference algorithm *)
(*
This code defines an empty environment gamma0 as a list of tuples (TypeVariable.t * 'a typ)
, and then adds the predefined types of the standard operations such as the 
concatenation operator for lists (@), the construction operator for lists (::), 
the construction operator for pairs (,), arithmetic binary operators 
*)
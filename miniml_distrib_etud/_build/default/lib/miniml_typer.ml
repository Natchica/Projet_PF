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

(* create an empty environment *)
let gamma0 : (TypeVariable.t * 'a typ) list = []

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

let gamma0 = [(TypeVariable.fraiche(),concat), (TypeVariable.fraiche(),cons), (TypeVariable.fraiche(),pair), 
(TypeVariable.fraiche(),arithop), (TypeVariable.fraiche(),relop), 
(TypeVariable.fraiche(),boolop), (TypeVariable.fraiche(),not), (TypeVariable.fraiche(),fst), 
(TypeVariable.fraiche(),snd), (TypeVariable.fraiche(),hd), (TypeVariable.fraiche(),tl)]

(* add a new type to the environment *)
let add (x, t) gamma = (x, t) :: gamma

(* find the type of a variable in the environment *)
let rec find x = function
  | [] -> raise Not_found
  | (y, t) :: gamma -> if TypeVariable.equal x y then t else find x gamma


(* Type inference algorithm *)

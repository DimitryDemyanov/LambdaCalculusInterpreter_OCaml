
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyString
  | TyArr of ty * ty
  | TyTupla of ty * ty
;;

type tcontext = (* Contexto de tipos *)
  (string * ty) list
;; 

type term =
    TmTrue
  | TmFalse
  | TmString of string
  | TmConcat of term * term
  | TmTupla of term * term
  | TmFirst of term
  | TmSecond of term
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
;;

(* Contexto de valores *)
type vcontext =
    (string * term) list
;;

type command =
    Eval of term
 |  Bind of string * term





(* CONTEXT MANAGEMENT *)

let emptytctx =
  []
;;

let addtbinding tctx x bind =
  (x, bind) :: tctx
;;

let gettbinding tctx x =
  List.assoc x tctx
;;

let emptyvctx =
  []
;;

let addvbinding vctx x bind =
  (x, bind) :: vctx
;;

let getvbinding vctx x =
  List.assoc x vctx
;;


(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyString -> 
      "String"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyTupla (ty1, ty2) ->
      "{" ^ string_of_ty ty1  ^ " * "  ^ string_of_ty ty2 ^ "}"
;;

exception Type_error of string
;;

let rec typeof tctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-String *)
  | TmString t1 -> 
      TyString
      
    (* T-Concat *)
  | TmConcat (t1, t2) -> 
	  if typeof tctx t1 = TyString then
		if typeof tctx t2 = TyString then TyString
		else raise (Type_error "Uno de los terminos a concatenar no es un string")
	  else raise (Type_error "Uno de los terminos a concatenar no es un string")
	  
	(* T-Tupla *)
  | TmTupla (t1, t2) -> 
      let tyT1 = typeof tctx t1 in
      let tyT2 = typeof tctx t2 in
      TyTupla (tyT1, tyT2)
 
    (* T-First *) 
  | TmFirst t1 ->
		(match typeof tctx t1 with
			TyTupla(tyT1,_) -> tyT1
		|   _ -> raise (Type_error "Error: Se espera una tupla"))
      
    (* T-Second *)
  | TmSecond t1 ->
		(match typeof tctx t1 with
			TyTupla(_,tyT2) -> tyT2
		|   _ -> raise (Type_error "Error: Se espera una tupla"))
    
    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof tctx t1 = TyBool then
        let tyT2 = typeof tctx t2 in
        if typeof tctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")
      
    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof tctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof tctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof tctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")
   
    (* T-Var *)
  | TmVar x ->
      (try gettbinding tctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let tctx' = addtbinding tctx x tyT1 in
      let tyT2 = typeof tctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof tctx t1 in
      let tyT2 = typeof tctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT2 = tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof tctx t1 in
      let tctx' = addtbinding tctx x tyT1 in
      typeof tctx' t2
    
    (* T-Fix *)  
  | TmFix t1 ->
      let tyT1 = typeof tctx t1 in
      (match tyT1 with
          TyArr(tyT11, tyT12) ->
              if tyT11 = tyT12 then tyT12
              else raise (Type_error "result of body not compatible with domain")
          | _ -> raise (Type_error "arrow type expected"))
            
;;


(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmString t1 ->
      "\"" ^ t1 ^ "\""	
  | TmConcat (t1,t2) ->
    "concat " ^ "(" ^ string_of_term t1 ^ ") (" ^ string_of_term t2 ^ ")" 
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmTupla (t1, t2) ->
      "{" ^ string_of_term t1 ^ "," ^ string_of_term t2 ^ "}"
  | TmFirst t1 ->
      "first " ^ "{" ^ string_of_term t1 ^ "}"
  | TmSecond t1 ->
      "second " ^ "{" ^ string_of_term t1 ^ "}"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t ->
	  "(fix " ^string_of_term t ^ ")"

;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmString t1->  
      []  
  | TmConcat (t1,t2)->  
      lunion (free_vars t1) (free_vars t2) 
  | TmTupla (t1,t2)->  
      lunion (free_vars t1) (free_vars t2)   
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmFirst t ->
      free_vars t
  | TmSecond t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
      free_vars t

;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;
    
let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmString t1 ->
      TmString t1
      
      
  | TmConcat (t1,t2) ->
	  TmConcat (subst x s t1, subst x s t2) 
	  
	  
  | TmTupla (t1,t2) ->
	  TmTupla (subst x s t1, subst x s t2) 
	  
	  
	  
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmFirst t ->
      TmFirst (subst x s t)
  | TmSecond t ->
      TmSecond (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) -> 
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))  
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t ->
      TmFix (subst x s t)
      

;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmTupla (t1,t2) -> isval t1 && isval t2
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 vctx tm = match tm with


			
    (* E-IfTrue *)
  | TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 vctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 vctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 vctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 vctx t1 in
      TmIsZero t1'
      
    (* E-Concat 1 *)
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2)

    (* E-Concat 2 *)
  | TmConcat (TmString s1, t2) ->
      let t2' = eval1 vctx t2 in
      TmConcat (TmString s1, t2')

    (* E-Concat 3 *)
  | TmConcat (t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmConcat (t1', t2)  

     
    (* E-Tupla *)
  | TmTupla (t1,t2) when isval t1 && isval t2 ->
     raise NoRuleApplies
    
    (* E-Pair2 *)
  | TmTupla (t1,t2) when isval t1 ->
     let t2' = eval1 vctx t2 in TmTupla (t1,t2')

    (* E-Pair1 *)
  | TmTupla (t1,t2) ->
     let t1' = eval1 vctx t1 in TmTupla (t1',t2)

    (* E-PairBeta1 *)
  | TmFirst (TmTupla (t1, _)) when isval t1 ->
      t1
      
    (* E-Proj1 *)
  | TmFirst t -> 
     let t' = eval1 vctx t in TmFirst t'
  
    (* E-PairBeta2 *)
  | TmSecond (TmTupla (_, t2)) when isval t2 ->
      t2
      
    (* E-Proj2 *)
  | TmSecond t -> 
     let t' = eval1 vctx t in TmSecond t'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 vctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmLetIn (x, t1', t2) 
      
    (* E-FixBeta *) 
  | TmFix v1 when isval v1 ->
	(match v1 with
		TmAbs (x, _, t12) -> subst x tm t12
		| _ -> raise NoRuleApplies)

    (* E-Fix *)
  | TmFix t1 ->
      let t1' = eval1 vctx t1 in
      TmFix t1'
      
  | TmVar s ->
     getvbinding vctx s

  | _ ->
      raise NoRuleApplies
;;



let rec apply_ctx vctx tm = match vctx with
    [] -> tm
  | (x,v)::t -> apply_ctx t (subst x v tm)
;;



let rec eval vctx tm =
  try
    let tm' = eval1 vctx tm in
    eval vctx tm'
  with
    NoRuleApplies -> apply_ctx vctx tm
;;

let execute (vctx, tctx) = function
    Eval tm ->
      let tyTm = typeof tctx tm in
      let tm' = eval vctx tm in
      print_endline ("- : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
      (vctx, tctx)
     
  | Bind (s, tm) -> 
      let tyTm = typeof tctx tm in
      let tm' = eval vctx tm in
      print_endline (s ^ " : " ^string_of_ty tyTm ^ " = " ^ string_of_term tm');
      (addvbinding vctx s tm', addtbinding tctx s tyTm)
;;
   

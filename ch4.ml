(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button. *)

type info = {row: int; col: int};;

type term =
    TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term;;

let rec isnumericval t = match t with
    TmZero(_) -> true
  | TmSucc(_, t1) -> isnumericval t1
  | _ -> false;;

let rec isval t = match t with
    TmTrue(_) -> true
  | TmFalse(_) -> true
  | t when isnumericval t -> true
  | _ -> false;;

exception NoRuleApplies

let rec eval1 t = match t with
    TmIf(_, TmTrue(_), t2, t3) ->
      t2
  | TmIf(_, TmFalse(_), t2, t3) ->
      t3
  | TmIf(fi, t1, t2, t3) -> 
      let t1' = eval1 t1 in 
      TmIf(fi, t1', t2, t3)
  | TmSucc(fi, t1) -> 
      let t1' = eval1 t1 in 
      TmSucc(fi, t1') 
  | TmPred(_, TmZero(fi)) -> 
      TmZero(fi)
  | TmPred(_, TmSucc(_, nv1)) when (isnumericval nv1) ->
      nv1
  | TmPred(fi, t1) -> 
      let t1' = eval1 t1 in
      TmPred(fi, t1')
  | TmIsZero(_, TmZero(fi)) -> 
      TmTrue(fi)
  | TmIsZero(_, TmSucc(fi, nv1)) when (isnumericval nv1) ->
      TmFalse(fi)
  | TmIsZero(fi, t1) ->
      let t1' = eval1 t1 in
      TmIsZero(fi, t1')
  | _ -> raise NoRuleApplies ;;

let rec eval2 t = match t with
    TmTrue(fi) -> t
  | TmFalse(fi) -> t
  | TmZero(fi) -> t
  | TmIf(fi, t1, t2, t3) -> 
      (match eval2 t1 with
       | TmTrue(_) -> eval2 t2
       | TmFalse(_) -> eval2 t3
       | _ -> raise NoRuleApplies)
  | TmSucc(fi, t1) when (isnumericval t1) ->
      let nv1 = eval2 t1 in
      TmSucc(fi, nv1)
  | TmPred(fi, t1) when (isnumericval t1) -> let t1' = eval2 t1 in 
      (match t1' with
       | TmZero(fi) -> t1'
       | TmSucc(fi, nv1) -> nv1
       | _ -> raise NoRuleApplies) 
  | TmIsZero(fi, t1) when (isnumericval t1) -> let t1' = eval2 t1 in 
      (match t1' with
       | TmZero(fi) -> TmTrue(fi)
       | TmSucc(fi, nv1) -> TmFalse(fi)
       | _ -> raise NoRuleApplies)
  | _ -> raise NoRuleApplies;;

;;

let rec eval t = 
  try let t' = eval1 t in
    eval t'
  with NoRuleApplies -> t;;

let a = {row = 1; col = 1};;
eval2 (TmSucc(a, TmSucc(a, TmZero(a))));;

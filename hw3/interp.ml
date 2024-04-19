open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds

(* Yuvan Rengifo*)
(* I pledge my honor that I have abided by the Stevens Honor system*)

(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> exp_val ea_result =
  fun e ->
  match e with
  | Int(n) ->
    return (NumVal n)
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1+n2))
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1-n2))
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1*n2))
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return (NumVal (n1/n2))
  | Let(id,def,body) ->
    eval_expr def >>= 
    extend_env id >>+
    eval_expr body 
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n = 0))
  | Pair(e1,e2) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= fun ev2 ->
    return (PairVal(ev1,ev2))
  | Fst(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun (l,_) ->
    return l
  | Snd(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun (_,r) ->
    return r
  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"

(*          HW3 Implementation               *)
  | IsEmpty (e) -> 
    eval_expr e >>= fun ev ->
      (match ev with
       | TreeVal Empty -> return (BoolVal true)
       | TreeVal _ -> return (BoolVal false)
       | _ -> error "Argument must be a tree")

  | EmptyTree (e) -> 
    return (TreeVal Empty)

  | Node (e1,e2,e3) -> 
      eval_expr e1 >>= fun n1 ->
      eval_expr e2 >>= fun n2 ->
      eval_expr e3 >>= fun n3 ->
      (match (n1, n2, n3) with
      | (_ , TreeVal l, TreeVal r) -> 
        return (TreeVal (Node(n1, l, r)))
      | _ -> error "Arguments must be a value and two trees")

  | CaseT (e1,e2,id1,id2,id3,e3) -> 
    eval_expr e1 >>= function
    | TreeVal Empty -> eval_expr e2
    | TreeVal (Node(v, l, r)) ->
        extend_env id1 v >>+
        extend_env id2 (TreeVal l) >>+
        extend_env id3 (TreeVal r) >>+
        eval_expr e3
    | _ -> error "CaseT expects a tree"

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e


(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c
  
(*   Yuvan Rengifo
     I pledge my honor that I have abided by the Stevens Honor System
     February 4, 2023
*)

(* ******************************************** *)
(** Basic functions on finite automata *)
(* ******************************************** *)
(**
   Code stub for assignment 1
*)

type symbol = char
type input = char list

type state = string

(* transition function *)
type tf = (state * symbol * state) list

(* initial state * transition function * end state *)
type fa = { states: state list; start:state; tf: tf; final: state list}


(* ******************************************** *)
(* Examples of automata *)
(* ******************************************** *)

let a = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1");("q1",'b',"q1"); ("q1",'c',"q2")];
         final = ["q2"]}

let a2 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1")
               ; ("q1",'c',"q2");  ("q3",'a',"q4")];
          final= ["q2"]
         }
let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]



(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

(* apply_transition_function : tf -> state -> symbol -> state option *)
let rec apply_transition_function f st sym : state option =                           (*When using this function:*)
  match f with                                                                        (*Specify which automaton is being tested*)
  | [] ->  None                                                                       (*Instead of f use: a.tf or a2.tf*)
  | (current_state, symbol, next_state)::rest ->(*Decompose tf to check matches*)     (*AKA name of automaton*)   (*own notes*)
      if current_state = st && symbol = sym then Some next_state
      else apply_transition_function rest st sym

(*-------------------------------------------------------------------------------------------------------------------------------------*)
(* Get the Automaton running *)
(* tf -> state -> input -> end_state*)
  let rec processor tf current_state input =                                          (*Created own helper function*)
    match input with                                                                  (*Didnt figure out how to implement*)
    | [] -> current_state (* Reached end *)                                           (*Given helper*)    (*own notes*)
    | symbol::rest ->
      match apply_transition_function tf current_state symbol with                                          
      | Some next_state -> processor tf next_state rest (* Continue with the next state if one found*)     
      | None -> current_state   (* No valid transition, stay*)                                              

(* Helper to check if a state is a final state *)    
(*final -> final: state list ->bool *)                                                         
  let is_final fin state =
   List.exists (fun final_state -> final_state = state) fin   (* Checks if it have the same state*)

(* accept : fa -> input -> bool *)
  let accept fa input =
    let input = input_of_string input in              (* Translate string to list of symbols*)
    let final_s = processor fa.tf fa.start input in   (* Pass params to processor to iterate the string*)
    is_final fa.final final_s  (* Checks if the last state is a final state*)
  
(*-------------------------------------------------------------------------------------------------------------------------------------*)
(* next : tf -> state -> symbol -> state list *)
let rec next tf st sym : state list =
  match tf with
  | [] -> []
  | (current_state, symbol, next_state)::rest ->
      if current_state = st && symbol = sym then
        next_state :: next rest st sym  (* Append transtion to list*)
      else
        next rest st sym  (* Keep checking for other successor states*)
  
(*-------------------------------------------------------------------------------------------------------------------------------------*)        
(*is_deterministic : fa -> bool*)
let is_deterministic fa =
  let rec check tr = (*recursive transition checker*)       (* Had to use an inner fucntion *)
    match tr with                                           (* that took tf as an input *)
    | [] -> true  (*deterministic *)                        (* Bc couldnt recursively call is_det*)
    | (state, symbol, _)::rest ->                           (* Since its a diff type of param*) (*own notes*)
      let det = next fa.tf state symbol in
      if List.length det > 1 then false  (* non-deterministic transition *)
      else check rest  (* Continue checking *)
  in
  check fa.tf     (*Access tf of certain automaton, return bool*)
    

(*-------------------------------------------------------------------------------------------------------------------------------------*)

(*dups: target -> state list -> bool*)
let rec dups e l =
  match l with
  | [] -> false              (*checks if head of list is the target*)
  | h::t -> (h=e) || dups e t (*Recursively checks the whole list, making the tail the new list*)


(*check_no__dups : state list -> bool*)
let rec check_no_dups st =
  match st with
  | [] -> true      (*no duplicates*)
  | h::t -> if dups h t then false else check_no_dups t (* If there is a dup, return false*)

(*check_start : start -> states -> bool*)
let check_start sst all =
  dups sst all 


(*check_fin : state list -> state list -> bool*)
let rec check_fin fin states =
  match fin with
  | [] -> true  (* All final states have been checked and are valid *)
  | h::t -> 
      if List.mem h states then check_fin t states (* Check if head is in states and recurse for tail *)
      else false  


(*valid : fa -> bool*)
let valid fa =
  (check_no_dups fa.states) &&
  (check_start fa.start fa.states) && 
  (check_fin fa.final fa.states) &&
  (is_deterministic fa)

(*-------------------------------------------------------------------------------------------------------------------------------------*)
(* Transitions to next state *)
let transitions_from tf state =
  List.fold_right (fun (start, sym, nxt) acc -> if start = state then nxt :: acc else acc) tf []
(* checks if the start state of the transition matches the given state*)

(*reachable : fa -> state list*)
let reachable fa =  (* Using DFS to check every visited state from the start*)
  let rec dfs rchd state =
    if List.mem state rchd then rchd
    else
      let new_visited = state :: rchd in  (*append reached states*)
      let next_states = transitions_from fa.tf state in
      List.fold_left (fun acc st -> dfs acc st) new_visited next_states
  in
  dfs [] fa.start

(*-------------------------------------------------------------------------------------------------------------------------------------*)
(*non_empty : fa -> bool*)
let non_empty fa =        (*checks that the final state is a reachable state*)
  let check = reachable fa in  (* showing that the fa will take in at least one string*)
  List.exists (fun state -> List.mem state fa.final) check (* at least one state in the list  of reachable states that is also a final state of the FA*)
(*-------------------------------------------------------------------------------------------------------------------------------------*)
(*do_removing: fa -> fa *)
let do_removing fa non_d =
  let removal_from lst =  (*filter the tf, checking if start and next are in reachable list*)
    List.filter (fun (start, sym, nxt) -> List.mem start non_d && List.mem nxt non_d) lst
  and remove_final lst =
    List.filter (fun f -> List.mem f non_d) lst (*takes in final states and checks if its reachable*)
  in
  let new_tf = removal_from fa.tf in            (*Creating*)
  let new_final = remove_final fa.final in        (*new*)
  let new_states = remove_final fa.states in       (*fa*)
  {states = new_states; start = fa.start; tf = new_tf; final = new_final }

(*remove_dead_states : fa ->fa*)
let remove_dead_states fa =
  let non_dead = reachable fa in 
  do_removing fa non_dead
(*-------------------------------------------------------------------------------------------------------------------------------------*)

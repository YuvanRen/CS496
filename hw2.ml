
(* Yuvan Rengifo
   I pledge my honor that I have abided by the Stevens honor system
   Cs496 hw2
*)

(* binary decision trees*)
type dTree =
| Leaf of int
| Node of char * dTree * dTree

let tLeft = Node('w', 
                  Node('x', Leaf 2, Leaf 5),
                  Leaf 8)

let tRight = Node('w', 
                  Node('x', Leaf 2, Leaf 5), 
                  Node('y', Leaf 7, Leaf 5))

(* height: dTree -> int *)
let rec height tree =
  match tree with
  | Leaf _ -> 1 
  | Node (_, left, right) ->
    let left_height = height left in
    let right_height = height right in
    1 + max left_height right_height
    
(* size: dTree -> int *)
let rec size tree =
  match tree with
  | Leaf x -> 1 
  | Node (x, left, right) ->
    1 + size left + size right;;

(* paths: dTree -> int list list *)
let rec paths tree =
  match tree with
  | Leaf _ -> [[]]
  | Node (_, left, right) ->
    let leftP = List.map (fun path -> 0 :: path) (paths left) in
    let rightP = List.map (fun path -> 1 :: path) (paths right) in
    leftP @ rightP

(*   Helper for is_perfect  *)
let rec total_nodes tree = 
  match tree with
  | Leaf _ -> 1
  | Node (_, left, right) -> 
    1 + total_nodes left + total_nodes right;;

(* is_perfect: dTree -> bool *)
let is_perfect tree =
  let h = height tree in
  let expected = (1 lsl h) - 1 in  (* bit shift, 2^(h) - 1 *)
  let actual = total_nodes tree in
  expected = actual
  
(* map: (f: char -> char, g: int -> int, t: dTree) -> NewdTree *)
let rec map f g t = 
  match t with
  | Leaf c -> Leaf (g c) 
  | Node (x, l, r) -> 
    Node (f x, map f g l, map f g r)

(* list_to_tree: char list-> dTree *)
let rec list_to_tree lst =
  match lst with
  | [] -> Leaf (0)
  | h::t -> Node(h, list_to_tree t, list_to_tree t)


(* Helper for replace_leaf_at*)
let replace leaf_val graph =
  let rec find list = match list with
    | [] -> None
    | (a, b)::t ->
      if List.mem leaf_val a then Some b
      else find t
  in
  match find graph with
  | Some replacement -> replacement
  | None -> leaf_val

(* replace_leaf_at: dTree -> (int list * int) list -> dTree *)
let rec replace_leaf_at tree lst =
  match tree with
  | Leaf x -> Leaf (replace x lst)
  | Node(x, l, r) ->
    Node(x, replace_leaf_at l lst, replace_leaf_at r lst);;

(* bf_to_dTree: char list -> (int list * int) list -> dTree *)
let bf_to_dTree cL lst=
  let newTree =  list_to_tree cL in
  replace_leaf_at newTree lst


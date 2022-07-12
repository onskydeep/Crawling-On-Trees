type tree = Empty 
          | Node of int * tree * tree
type command = Left | Right | Up | New of int | Delete | Push | Pop

(* print a graphical representation (dot) of a binary tree (2. argument) to a file (1. argument) *)
let print_tree filename btree = 
  let file = open_out filename in
  Printf.fprintf file "digraph Tree {\n";
  let rec print next_id = function Empty -> 
    Printf.fprintf file "\tn%d[shape=rectangle,label=\"\"];\n" next_id; next_id + 1, next_id
  | Node (x, l, r) ->
    let node_id = next_id in
    Printf.fprintf file "\tn%d[label=\"%d\"];\n" node_id x;
    let next_id, lid = print (next_id + 1) l in
    let next_id, rid = print next_id r in 
    (Printf.fprintf file "\tn%d -> n%d[label=\"L\"];\n" node_id lid);
    (Printf.fprintf file "\tn%d -> n%d[label=\"R\"];\n" node_id rid);
    next_id, node_id
  in
  ignore(print 0 btree);
  Printf.fprintf file "}";
  close_out file

type tree = Empty | Node of int * tree * tree ;;

type command = Left | Right | Up | New of int | Delete | Push | Pop ;; 

type parent = Empty | Pair of tree * command ;;

let rec crawl_2 command_list current_node parents stack= 
  match command_list with
  | [] -> (
      match parents with 
      | [] -> current_node
      | Pair(parent_node, move)::rest_parents -> ( 
          match (parent_node,move) with
          | (Node(value, left, right), Left) -> crawl_2 [] (Node(value, current_node, right)) rest_parents stack
          | (Node(value, left, right), Right) -> crawl_2 [] (Node(value, left, current_node)) rest_parents stack
        ) 
    )
  | cmd :: rest_command_list-> (
      match cmd with
      | Left -> ( 
          match current_node with
          | Node( _ , Empty , _ ) -> raise (Failure "No left child")
          | Node( value , left, right) -> crawl_2 rest_command_list left (Pair(current_node,Left)::parents) stack
        )
      | Right -> ( 
          match current_node with
          | Node( _ , _ , Empty ) -> raise (Failure "No right child")
          | Node( value , left, right) -> crawl_2 rest_command_list right (Pair(current_node,Right)::parents) stack
        )
      | Up -> (
          match parents with
          | [] -> raise (Failure "You are at the root, no parent there!") 
          | Pair(parent_node, _) :: rest_parents -> crawl_2 rest_command_list parent_node rest_parents stack
        ) 
      | New node_val -> ( 
          crawl_2 rest_command_list (Node(node_val, Empty, Empty)) parents stack
        )
      | Delete -> (
          crawl_2 rest_command_list Empty parents stack
        )
      | Push -> (
          crawl_2 rest_command_list current_node parents (current_node::stack)
        )  
      | Pop ->(
          match stack with
          | [] -> raise (Failure "Nothing to remove from stack")
          | tail::rest_stack -> crawl_2 rest_command_list tail parents rest_stack      
        )  
    );;

let crawl command_list tree= crawl_2 command_list tree [] [];; 

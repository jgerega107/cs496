(* 

   Stub for HW2 
   Please
   1. Rename to gt.ml
   2. Place the names of the group members here:

    Name1:
    Name2:

*)



type 'a gt = Node of 'a*('a gt) list

let mk_leaf (n:'a) : 'a gt =
  Node(n,[])
    
let t : int gt =
 Node (33,
       [Node (12,[]);
        Node (77, 
              [Node (37, 
                     [Node (14, [])]); 
               Node (48, []); 
               Node (103, [])])
       ])


let rec height t =
  match t with
  | Node(_, []) -> 1
  | Node(_, children) -> 1 + List.fold_left (fun x y -> (max x (height y))) 0 children
    
let rec size t =
  match t with
  | Node(_, []) -> 1
  | Node(_, children) -> 1 + List.fold_left (fun x y -> (x + (size y))) 0 children

let rec paths_to_leaves t =
  match t with
  | Node(_, []) -> []
  | Node(_, h::tl) -> [0] :: paths_to_leaves_h tl 1

let rec is_perfect t =
  failwith "implement"


let rec preorder (Node(d,ch)) =
    failwith "implement"

                        
let rec mirror (Node(d,ch)) =
  failwith "implement"

  
let rec mapt f (Node(d,ch)) =
  failwith "implement"
  
let rec foldt : ('a -> 'b list -> 'b) -> 'a gt -> 'b =
  fun f (Node(d,ch)) ->
  failwith "implement"

let sumt t =
  foldt (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t

let memt t e = 
  foldt (fun i rs -> i=e || List.exists (fun i -> i) rs) t

let mirror' t  = 
  failwith "implement"

(*Jacob Gerega and James Labayna*)

type 'a bt = Empty | Node of 'a * 'a bt * 'a bt

let t : int bt =
Node(12,
Node(7,Empty,Empty),
Node(33,
Node(24,Empty,Empty),
Empty))

let rec sizet (t:'a bt) : int =
match t with
| Empty -> 0
| Node(d,lt,rt) -> 1 + sizet lt + sizet rt

let rec height t =
  match t with
  | Empty -> 0
  | Node(_, lt, rt) -> 1+max (height lt) (height rt)

let perfect t =
  match t with
  | Empty -> false
  | Node(_, Empty, Empty) -> true
  | Node(_, lt, rt) -> height lt == height rt

let rec prune (n:int) (t:'a bt) : 'a bt =
  match n,t with
  | n,_ when (n < 0) -> failwith "Undefined behavior"
  | n,_ when ((n = 0) || (n > (height t))) -> Empty
  | n,Node(d,l,r) -> Node(d,(prune (n-1) l),(prune (n-1) r))
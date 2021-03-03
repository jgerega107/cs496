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

let rec sumt (t:int bt) : int =
match t with
| Empty -> 0
| Node(d,lt,rt) -> d + sumt lt + sumt rt

let rec height t =
match t with
| Empty -> 0
| Node(_, lt, rt) -> 1+max (height lt) (height rt)

let rec is_leaf t =
  match t with
  | Node(_,Empty,Empty) -> true
  | _ -> false

let perfect t =
  match t with
  | Empty -> false
  | Node(_, Empty, Empty) -> true
  | Node(_, lt, rt) -> sizet lt == sizet rt

let rec prune n t =
  if n > (height t)
  then t
  else
  match t,n with
  | _,n when n <= 0 -> Empty
  | Node(d,Empty,r),n -> Node(d,Empty,(prune (n-1) r))
  | Node(d,l,Empty),n -> Node(d,(prune (n-1) l),Empty)
  | Node(d,l,r),n -> Node(d,(prune (n-1) l),(prune (n-1) r))
let rec fact n = 
	match n with
	| 0 -> 1
	| m -> m * fact (m-1)

let rec mem e l =
	match l with
	| [] -> false
	| h::t -> if h=e then true else mem e t

let rec rev l =
	match l with
	| [] -> []
	| h::t -> rev t @ [h]

	(*remember that the @ sign is append and :: is cons*)

let rec stutter l =
	match l with
	| [] -> []
	| h::t -> h::h::stutter t

let has_size_at_least_two l =
	match l with
	| _::_::_ -> true
	| _ -> false

let rec succl l =
	match l with
	| [] -> []
	| h::t -> h+1::succl t

let rec greater_than_zero l =
	match l with
	| [] -> []
	| h::t -> if h>0 then h :: greater_than_zero t else greater_than_zero t

let rec andl l =
	match l with
	| [] -> true
	| h::t -> h && andl t

let rec map f l =
	match l with
	| [] -> []
	| h::t -> f h :: map f t

let isne i = i<>[]

let rec filter p l = 
	match l with
	| [] -> []
	| h::t -> if p then h :: filter p t else filter p t


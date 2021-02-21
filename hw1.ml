type program = int list

let letter_e = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]

let mirror c = 
  match c with
  | 2 -> 4
  | 3 -> 5
  | 4 -> 2
  | 5 -> 3
  | m -> m

let rotate90 c = 
  match c with
  | 2 -> 3
  | 3 -> 4
  | 4 -> 5
  | 5 -> 2
  | m -> m

let mirror_image l =
  match l with
  | [] -> []
  | h::t -> List.map mirror l

let rotate_90_letter l =
  match l with
  | [] -> []
  | h::t -> List.map rotate90 l

let rotate_90_word l =
  match l with
  | [] -> []
  | h::t -> List.map rotate_90_letter l

let rec repeat x c =
  match x with
  | 0 -> []
  | _ -> c :: repeat (x-1) c

let rec repeat' x c =
  match x with
  | 0 -> []
  | _ -> if c!= 0 && c!=1 then c :: repeat' (x-1) c else [c]

let rec pantograph_nm x l =
  match l with
  | [] -> []
  | h::t -> repeat' x h @ pantograph_nm x t

let pantograph x l = List.concat_map (fun l -> repeat' x l) l

let pantograph_fold x l = List.fold_left (fun a b -> a @ (repeat' x b)) [] l 

let direction c t =
  match c with
  | 2 -> (fst t, (snd t)+1)
  | 3 -> ((fst t)+1, snd t)
  | 4 -> (fst t, (snd t)-1)
  | 5 -> ((fst t)-1, snd t)
  | _ -> (fst t, snd t)

let rec coverage' p l =
  match l with
  | [] -> []
  | h::t -> direction h p :: coverage' (direction h p) t

let coverage p l = p :: coverage' p l

let rec compressc d c l =
  match l with
  | [] -> [(d, c)]
  | h::t -> if h=d then (compressc d (c+1) t) else (d, c) :: (compressc h 1 t)

let compress l = compressc 0 0 l

let rec uncompressh c p =
  match c with
  | 1 -> [p]
  | _ -> p :: uncompressh (c-1) p

let rec uncompress l = 
  match l with
  | [] -> []
  | h::t -> if (snd h)>1 then (uncompressh (snd h) (fst h)) @ uncompress t else (fst h) :: uncompress t



type program = int list

let letter_e = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]

(*Jacob Gerega*)
(*I pledge my honor that I have abided by the Stevens Honor System.*)
(*2/20/21*)

(*switch function to define encoding for later functions*)
let mirror c = 
  match c with
  | 2 -> 4
  | 3 -> 5
  | 4 -> 2
  | 5 -> 3
  | m -> m

(*switch function to define encoding for later functions*)
let rotate90 c = 
  match c with
  | 2 -> 3
  | 3 -> 4
  | 4 -> 5
  | 5 -> 2
  | m -> m

  (*function that takes in a list and uses encoding function to return mirrored encoding*)
let mirror_image l =
  match l with
  | [] -> []
  | h::t -> List.map mirror l

  (*function that takes in a list and uses encoding function to return rotated encoding*)
let rotate_90_letter l =
  match l with
  | [] -> []
  | h::t -> List.map rotate90 l

  (*function that takes in a list of letters and uses encoding function to return rotated encoding*)
let rotate_90_word l =
  match l with
  | [] -> []
  | h::t -> List.map rotate_90_letter l

  (*function that takes in a number and a word and returns a list with x many occurances of word c*)
let rec repeat x c =
  match x with
  | 0 -> []
  | _ -> c :: repeat (x-1) c

  (*modified version of repeat that handles 0's and 1's for pantograph*)
let rec repeat' x c =
  match x with
  | 0 -> []
  | _ -> if c!= 0 && c!=1 then c :: repeat' (x-1) c else [c]

  (*funciton that takes in a number and a list and returns every element of that list (except 0's and 1's) repeated x times*)
let rec pantograph_nm x l =
  match l with
  | [] -> []
  | h::t -> repeat' x h @ pantograph_nm x t

 (*funciton that takes in a number and a list and returns every element of that list (except 0's and 1's) repeated x times*)
let pantograph x l = List.concat_map (fun l -> repeat' x l) l

 (*funciton that takes in a number and a list and returns every element of that list (except 0's and 1's) repeated x times*)
let pantograph_f x l = List.fold_left (fun a b -> a @ (repeat' x b)) [] l 

 (*switch function that returns a new coordinate based on encoding*)
let direction c t =
  match c with
  | 2 -> (fst t, (snd t)+1)
  | 3 -> ((fst t)+1, snd t)
  | 4 -> (fst t, (snd t)-1)
  | 5 -> ((fst t)-1, snd t)
  | _ -> (fst t, snd t)

  (*helper function for coverage which takes in a starting point and a list of points and returns every point that is visited*)
let rec coverage' p l =
  match l with
  | [] -> []
  | h::t -> direction h p :: coverage' (direction h p) t
  (*main function for coverage*)
let coverage p l = p :: coverage' p l

(*compress helper that takes in a count, a direction, and a list of encodings*)
let rec compressc d c l =
  match l with
  | [] -> [(d, c)]
  | h::t -> if h=d then (compressc d (c+1) t) else (d, c) :: (compressc h 1 t)

(*main function for compress*)
let compress l = compressc 0 0 l

(*helper function that reverses compression by reading count in a point and repeating the encoding*)
let rec uncompressh c p =
  match c with
  | 1 -> [p]
  | _ -> p :: uncompressh (c-1) p

  (*main function of uncompress that takes in a compressed list of encodings and counts and uncompresses it to a list of repeated encodings*)
let rec uncompress l = 
  match l with
  | [] -> []
  | h::t -> if (snd h)>1 then (uncompressh (snd h) (fst h)) @ uncompress t else (fst h) :: uncompress t

    (*helper function of uncompress_map*)
let uncompress_mh p = if (snd p)>1 then (uncompressh (snd p) (fst p)) else [fst p]

  (*main function of uncompress that takes in a compressed list of encodings and counts and uncompresses it to a list of repeated encodings*)
let uncompress_m l = List.concat_map uncompress_mh l

  (*main function of uncompress that takes in a compressed list of encodings and counts and uncompresses it to a list of repeated encodings*)
let uncompress_f l = List.fold_left (fun a b -> a @ (uncompress_mh b)) [] l 

  (*helper function of optimize that reads the current mode and compares it to the list, removing redundant pen ups/downs*)
let rec optimizeh cm l = 
  match l with
  | [] -> []
  | h::t -> if cm=h && (cm=0 || cm=1) then optimizeh cm t else h :: optimizeh h t

  (*main function of optimize*)
let optimize l = optimizeh 1 l;




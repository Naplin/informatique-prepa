type chaine = char list;;
type corde = | V | F of int * chaine | N of int * corde * corde;;

let length c = match c with
	| V -> 0
	| F (k, _) -> k
	| N (k, _, _) -> k;;

let corde_of_chaine c = let rec taille c = match c with
	| [] -> 0
	| h::t -> 1 + taille t
	in let k = taille c in if k > 0 then F (k, c) else V;;

let concat c1 c2 = N (length c1 + length c2, c1, c2);;

let rec nth n c = match c with
	| V -> None
	| F (k, h::t) -> (match n with 
		| 0 -> Some h
		| _ -> nth (n-1) (corde_of_chaine t))
	| N (k, c1, c2) -> let a = length c1 in if a > n then nth n c1 else nth (n-a) c2;;

let rec sous_corde c a b = match c with
	| V -> None
	| F (k, h::t) -> (match a with)

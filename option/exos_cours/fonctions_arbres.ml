type 'a general = 
    |N of 'a * 'a general list;;

type 'a general_opt = 
    |None
    |Some of 'a general

let etiquette_racine a = match a with
    |N(e, _) -> e;;

let liste_fils a = match a with
    |N(_, l) -> l;;

let contient a x = let rec aux a x = match a with
    |[] -> false
    |N(e, l)::t -> x = e || aux t x || aux l x
    in aux [a] x;;

let rec sous_arbre_opt a x = let rec aux l = match l with
    |[] -> None
    |h::t -> match sous_arbre_opt h x with
        |None -> aux t
        |Some b -> Some b
    in match a with
        |N(e, l) -> if x = e then Some a else aux l;;

let rec descendant_gauche a = match a with
    |N(e, []) -> e
    |N(e, h::t) -> descendant_gauche h;;



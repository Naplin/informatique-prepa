type 'a arbre =
    |V
    |N of 'a * 'a arbre * 'a arbre;;

type 'a arbre_strict =
    |F of 'a
    |N of 'a * 'a arbre_strict * 'a arbre_strict;;

let rec hauteur a = match a with
    |F(_) -> 0
    |N(_, l, r) -> 1 + max (hauteur l) (hauteur r);;

let rec taille a = match a with
    |F(_) -> 1
    |N(_, l, r) -> 1 + taille l + taille r;;

let rec est_strict a = match a with
    |V -> true
    |N(_, l, r) -> match l, r with
        |V, N(_) | N(_), V -> false
        |_ -> est_strict l && est_strict r;;



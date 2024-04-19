type 'a arbre_general =
    |Vide
    |Noeud of 'a * 'a foret and
    'a foret = 'a arbre_general list;;


let a = Noeud(5, [Noeud(2, []); Noeud(1, [Noeud(3, []); Noeud(8, []); Noeud(9, [])]); Noeud(7, [Noeud(6, [])])])

let rec taille a = match a with
    |Vide -> 0
    |Noeud(_, f) -> 1 + taille_foret f
and taille_foret f = match f with
    |[] -> 0
    |h::t -> taille h + taille_foret t;;

let rec hauteur a = match a with
    |Vide -> -1
    |Noeud(_, f) -> 1 + max_hauteur_foret f
and max_hauteur_foret f = match f with
    |[] -> -1
    |h::t -> max (hauteur h) (max_hauteur_foret t);;

let rec taille_liste l = match l with
    |[] -> 0
    |_::t -> 1 + taille_liste t;;

let rec degre a = match a with
    |Vide -> 0
    |Noeud(_, f) -> max (taille_liste f) (degre_foret f)
and degre_foret f = match f with
    |[] -> 0
    |h::t -> max (degre h) (degre_foret t);;


(*Type enregistrement*)

type 'a arbre_general = {etiquette : 'a; fils : 'a arbre_general list}

let rec taille a = match a.fils with
    |[] -> 1
    |_ -> 1 + taille_fils a.fils
and taille_fils f = match f with
    |[] -> 0
    |h::t -> taille h + taille_fils t;;

let rec hauteur a = match a.fils with
    |[] -> 0
    |_ -> 1 + max_hauteur_fils a.fils
and max_hauteur_fils f = match f with
    |[] -> 0
    |h::t -> max (hauteur h) (max_hauteur_fils t);;

let rec nb_feuilles a = match a.fils with
    |[] -> 1
    |_ -> nb_feuilles_fils a.fils
and nb_feuilles_fils f = match f with
    |[] -> 0
    |h::t -> nb_feuilles h + nb_feuilles_fils t;;

let rec max_etiquettes a = match a.fils with
    |[] -> a.etiquette
    |_ -> max_etiquettes_fils a.fils a.etiquette
and max_etiquettes_fils f ma = match f with
    |[] -> ma
    |h::t -> let ma_h = max_etiquettes h in max ma_h (max_etiquettes_fils t ma_h);;

let max_somme_branche a = let rec aux a s = match a.fils with
    |[] -> a.etiquette + s
    |_ -> max_somme_branche_fils a.fils (s + a.etiquette)
and max_somme_branche_fils f ma = match f with
    |[] -> ma
    |h::t -> let ma_h = aux h ma in max ma_h (max_somme_branche_fils t ma)
    in aux a 0;;

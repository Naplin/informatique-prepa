type 'a t = V|N of 'a * 'a t * 'a t

let rec hauteur a = match a with
    |V -> -1
    |N(_, l, r) -> 1 + max (hauteur l) (hauteur r);;

let rec taille a = match a with
    |V -> 0
    |N(_, l, r) -> 1 + taille l + taille r;;

let rec arbre_complet h = match h with
    |0 -> N(1, V, V)
    |_ -> let n = arbre_complet (h-1) in N(1, n, n);;

let est_feuille a = match a with
    |N(_, V, V) -> true
    |_ -> false;;

let rec somme_feuilles a = match a with
    |V -> 0
    |N(e, l, r) -> let x = somme_feuilles l + somme_feuilles r in if est_feuille a then 1 + x else x;;

let rec min_max a = match a with
    |V -> failwith "arbre vide"
    |N(e, V, V) -> e, e 
    |N(e, f, V) | N(e, V, f) -> let mi, ma = min_max f in min e mi, max e ma 
    |N(e, l, r) -> let mi_l, ma_l = min_max l in let mi_r, ma_r = min_max r in min e (min mi_l mi_r), max e (max ma_l ma_r);;

let max_somme_branche a = let rec aux a s = match a with
    |V -> failwith "arbre vide"
    |N(e, V, V) -> e+s
    |N(e, f, V) |N(e, V, f) -> aux f (s+e)
    |N(e, l, r) -> max (aux l (s+e)) (aux r (s+e)) in
    aux a 0;;


(* Partie arbres stricts *)

type ('a, 'b) arbre_strict = 
    |Feuille of 'a
    |Noeud_interne of 'b * ('a, 'b) arbre_strict * ('a, 'b) arbre_strict;;

let est_feuille_st a = match a with
    |Feuille _ -> true
    |_ -> false;;

let rec nb_feuilles_st a = match a with
    |Feuille _ -> 1
    |Noeud_interne(e, l, r) -> nb_feuilles_st l + nb_feuilles_st r;;

let rec somme_etiquettes_st a = match a with
    |Feuille e -> e
    |Noeud_interne(e, l, r) -> e + somme_etiquettes_st l + somme_etiquettes_st r;;

let rec somme_feuilles_st a = match a with
    |Feuille e -> e
    |Noeud_interne(e, l, r) -> somme_feuilles_st l + somme_feuilles_st r;;

let rec min_max_st a = match a with
    |Feuille e -> e, e
    |Noeud_interne(e, l, r) -> let mi_l, ma_l = min_max_st l in let mi_r, ma_r = min_max_st r in min mi_l mi_r, max ma_l ma_r;;

let max_somme_branche_st a = let rec aux a s = match a with
    |Feuille e -> e
    |Noeud_interne(e, l, r) -> max (aux l (s+e)) (aux r (s+e)) in
    aux a 0;;


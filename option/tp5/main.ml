(* Ex1 *)

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

(* Ex2 *)

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

(* Ex3 *)

type ('a, 'b) arbre_strict =
    |Feuille of 'a
    |Noeud of 'b * ('a, 'b) arbre_strict * ('a, 'b) arbre_strict;;

type ('a, 'b) token =
    |F of 'a
    |N of 'b;;

let a = Noeud('A', Noeud('B', Feuille 'D', Noeud('E', Noeud('H', Feuille 'I', Feuille 'J'), Feuille 'K')), Noeud('C', Feuille 'F', Feuille 'G'))

let parcours_postfixe a = let rec aux a l = match a with
    |Feuille e -> F e::l
    |Noeud(e, g, d) -> aux g (aux d (N e::l))
    in aux a [];;

let parcours_prefixe a = let rec aux a l = match a with
    |Feuille e -> F e::l
    |Noeud(e, g, d) -> N e::aux g (aux d l)
    in aux a [];;

let parcours_infixe a = let rec aux a l = match a with
    |Feuille e -> F e::l
    |Noeud(e, g, d) -> aux g (N e::(aux d l))
    in aux a [];;

(* Ex4 *)

type ('a, 'b) arbre_strict =
    |Feuille of 'a
    |Noeud of 'b * ('a, 'b) arbre_strict * ('a, 'b) arbre_strict;;

type ('a, 'b) token =
    |F of 'a
    |N of 'b;;

let a = Noeud('A', Noeud('B', Feuille 'D', Noeud('E', Noeud('H', Feuille 'I', Feuille 'J'), Feuille 'K')), Noeud('C', Feuille 'F', Feuille 'G'))

let parcours_postfixe a = let rec aux a l = match a with
    |Feuille e -> F e::l
    |Noeud(e, g, d) -> aux g (aux d (N e::l))
    in aux a [];;

let reconstruit_postfixe l = let rec aux l a = match l with 
    |[] -> (match a with
        |[] -> failwith "parcours invalide"
        |h::t -> h)
    |F e::t -> aux t (Feuille e::a)
    |N e::t -> (match a with
        |[] |[_] -> failwith "parcours invalide"
        |x::y::t' -> aux t (Noeud(e, y, x)::t'))
    in aux l [];;

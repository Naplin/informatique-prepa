type 'a arbre =
        | Vide
        | Noeud of 'a arbre * 'a * 'a arbre;;

let rec nombre_feuilles arbre = match arbre with
        | Vide -> 0
        | Noeud(Vide, _, Vide) -> 1
        | Noeud(l, _, r) -> nombre_feuilles l + nombre_feuilles r;;

let a = Noeud(Noeud(Noeud(Vide, 5, Vide), 12, Noeud(Vide, 8, Vide)), 25, Noeud(Noeud(Noeud(Vide, 16, Vide), 52, Noeud(Vide, 27, Vide)), 2, Noeud(Vide, 10, Vide)));;

let rec prefixe arbre = match arbre with
        | Vide -> []
        | Noeud(Vide, x, Vide) -> [x]
        | Noeud(l, x, r) -> x::prefixe l@prefixe r;;



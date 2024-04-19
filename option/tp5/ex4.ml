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

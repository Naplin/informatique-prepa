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

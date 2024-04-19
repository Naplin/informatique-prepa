type 'a abr =
    |V
    |N of 'a abr * 'a * 'a abr;;

let rec recherche a x = match a with
    |V -> false
    |N(g, e, d) -> e = x || if x < e then recherche g x else recherche d x;;

let rec min_abr a = match a with
    |V -> failwith "arbre vide"
    |N(V, e, _) -> e
    |N(g, e, _) -> min_abr g;;

let rec max_abr a = match a with
    |V -> failwith "arbre vide"
    |N(_, e, V) -> e
    |N(_, e, d) -> max_abr d;;

let rec est_abr_min_max a = match a with
    |V |N(V, _, V) -> true
    |N(g, e, V) -> est_abr_min_max g && max_abr g < e
    |N(V, e, d) -> est_abr_min_max d && min_abr d > e
    |N(g, e, d) -> est_abr_min_max g && est_abr_min_max d && max_abr g < e && min_abr d > e;;

let rec insere a x = match a with
    |V -> N(V, x, V)
    |N(g, e, d) -> if e = x then a else if x < e then N(insere g x, e, d) else N(g, e, insere d x);;

let rec abr_of_list l = match l with
    |[] -> V
    |h::t -> insere (abr_of_list t) h;;

let parcours_infixe a = let rec aux a l = match a with
    |V -> []
    |N(V, e, V) -> e::l
    |N(V, e, d) -> e::aux d l
    |N(g, e, V) -> aux g (e::l)
    |N(g, e, d) -> aux g (e::aux d l)
    in aux a [];;

let trie l = parcours_infixe (abr_of_list l);;
(*Il faut que la liste soit sans doublons!*)

(* Ex1 *)

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

let trie l = parcours_infixe (abr_of_list l);
(*Il faut que la liste soit sans doublons!*)

(* Ex2 *)

type 'a arbre =
    |V
    |N of 'a arbre * 'a * 'a arbre;;

let rec supprime a x = match a with
    |V -> V
    |N(V, e, d) when e = x -> d
    |N(g, e, V) when e = x -> g
    |N(g, e, d) when e < x -> N(g, e, supprime d x)
    |N(g, e, d) when e > x -> N(supprime g x, e, d)
    |N(g, e, d) -> let rec aux g d = match d with
        |V -> g
        |N(g', e, d') -> N(aux g d', e, d)
    in aux g d;;

let rec supprime_min a = match a with
    |V -> failwith "arbre vide"
    |N(g, e, d) -> let mi, g' = supprime_min g in mi, N(g', e, d);;

let supprime_v2 a x = match a with
    |V -> V
    |N(V, e, d) when e = x -> d
    |N(g, e, V) when e = x -> g
    |N(g, e, d) when e < x -> N(g, e, supprime d x)
    |N(g, e, d) when e > x -> N(supprime g x, e, d)
    |N(g, e, d) -> let mi, d' = supprime_min d in N(g, mi, d');;

(* Ex4 *)

type ('a, 'b) dict =
    |V
    |N of 'a * 'b * ('a, 'b) dict * ('a, 'b) dict;;

(*Q1*)
let vide = V

let rec ajouter a b dict = match dict with
    |V -> N(a, b, V, V)
    |N(x, y, g, d) -> if a = x then failwith "cle deja presente" else if a < x then N(x, y, ajouter a b g, d) else N(x, y, g, ajouter a b d);;

let rec supprime_min dict = match dict with
    |V -> failwith "dict vide"
    |N(x, y, V, d) -> x, y, d
    |N(x, y, g, d) -> let mix, miy, g' = supprime_min g in mix, miy, N(x, y, g', d);;

let rec supprime a dict = match dict with
    |V -> V
    |N(x, _, V, f) |N(x, _, f, V) when a = x -> f
    |N(x, y, g, d) when a < x -> N(x, y, supprime a g, d)
    |N(x, y, g, d) when a > x -> N(x, y, g, supprime a d)
    |N(x, y, g, d) -> let mix, miy, g' = supprime_min g in N(mix, miy, g', d);;

let rec find_opt a dict = match dict with
    |V -> None
    |N(x, y, g, d) -> if a = x then Some y else if a < x then find_opt a g else find_opt a d;;


(*EX5, multiensembles*)

let rec ajouter_mult a dict = match dict with
    |V -> N(a, 1, V, V)
    |N(x, y, g, d) -> if a = x then N(x, y+1, g, d) else if a < x then N(x, y, ajouter_mult a g, d) else N(x, y, g, ajouter_mult a d);;

let rec supprime_mult a dict = match dict with
    |V -> V
    |N(x, y, g, d) -> if a = x then if y = 1 then supprime a dict else N(x, y-1, g, d) else if a < x then N(x, y, supprime_mult a g, d) else N(x, y, g, supprime_mult a d);;

let rec nb_occurences a dict = match dict with
    |V -> 0
    |N(x, y, g, d) -> if a = x then y else if a < x then nb_occurences a g else nb_occurences a d;;

let rec taille_mult dict = match dict with
    |V -> 0
    |N(_, y, g, d) -> y + taille_mult g + taille_mult d;;

let rec nth_plut_petit n dict = match dict with
    |V -> failwith "dict vide"
    |N(x, y, g, d) -> 0;;(*TODO, comprends pas la question!*)

(* Ex5 *)

type 'a abr =
    |V
    |N of 'a * 'a abr * 'a abr;;

let rec separe a x = match a with
    |V -> V, V
    |N(e, g, d) -> if e = x then g, N(e, V, d) else if x > e then let g', d' = separe d x in N(e, g, g'), d' else let g', d' = separe g x in g', N(e, d', d);;

let arbre = N(4, N(2, V, N(3, V, V)), N(8, N(6, N(5, V, V), N(7, V, V)), N(9, V, V)))
let arbre_non_abr = N(4, N(3, V, N(2, V, V)), N(8, N(6, N(5, V, V), N(7, V, V)), N(9, V, V)))

let est_abr a = let rec infixe a l = (match a with
    |V -> l
    |N(e, g, d) -> infixe g (e::infixe d l)) in
    let rec est_triee l = (match l with
    |[] |[_] -> true
    |a::b::t -> a < b && est_triee (b::t)) in
    est_triee (infixe a []);;

(* Ex1 *)

let rec puissance x n = match n with
    |0 -> 1
    |_ -> x * puissance x (n-1);;

let rec puissance_rapide x n = match n with
    |0 -> 1
    |_ -> if n mod 2 = 0 then let k = puissance x (n/2) in k*k else let k = puissance x ((n-1)/2) in x*k*k;;

let rec somme_n_puissance_k n k = match n with
    |0 -> 0
    |_ -> puissance n k + somme_n_puissance_k (n-1) k;;

let rec binom k n = match n with
    |0 -> 0
    |_ -> match k with
        |0 -> 1
        |_ -> binom (k-1) (n-1) + binom k (n-1);;

let rec somme_liste l = match l with
    |[] -> 0
    |h::t -> h + somme_liste t;;

let croissant l = match l with
    |[] -> true
    |h::t -> let rec aux p l = match l with
        |[] -> true
        |h'::t' -> if p <= h' then aux h' t' else false
    in aux h t;;

let compare_taille a b = let rec taille l = match l with
    |[] -> 0
    |h::t -> 1 + taille t
    in taille b - taille a;;

(* Ex2 *)

let rec insere l x = match l with
    |[] -> [x]
    |h::t -> if h <= x then h::insere t x else x::h::t;;

let rec tri_insertion l = match l with
    |[] -> []
    |h::t -> insere (tri_insertion t) h;;
(*

Dans le pire des cas, insere compare n fois, tri_insertion compare n**2 fois
Dans le meilleur des cas, insere compare 1 fois, tri_insertion compare n fois

*)

(* Ex3 *)

let divise l = let rec aux l l1 l2 = match l with
    |[] -> l1, l2
    |h::t -> aux t (h::l2) l1
    in aux l [] [];;

let rec fusionne l1 l2 = match l1, l2 with
    |[], t |t, [] -> t
    |h1::t1, h2::t2 -> if h1 <= h2 then h1::fusionne t1 l2 else h2::fusionne l1 t2;;

let rec tri_fusion l = let l1, l2 = divise l in match l1, l2 with
    |[], t |t, [] -> t
    |_ -> fusionne (tri_fusion l1) (tri_fusion l2);;

(*

Dans le pire cas, tri_fusion compare 2**n*(n+n)
Dans le meilleur cas, idem (?)

*)

(* Ex4 *)

type monome = {coeff : int; degre : int};;
type polynome = monome list;;

let p : polynome = [
    {coeff = -1; degre = 10};
    {coeff = 3; degre = 2};
    {coeff = 2; degre = 1}
];;

let rec polynome_valide p = match p with
    |[] -> true
    |[h] -> h.degre > 0 && h.coeff <> 0
    |h::h'::t -> if h.degre < h'.degre || h.degre < 0 || h.coeff = 0 then false else polynome_valide (h'::t);;



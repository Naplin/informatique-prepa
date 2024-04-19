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



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

type 'a arbre =
    |V
    |N of 'a arbre * 'a * 'a arbre;;

let rec supprime a x = match a with
    | V -> V
    | N (V, e, d) when e = x -> d
    | N (g, e, V) when e = x -> g
    | N (g, e, d) when e < x -> N(g, e, supprime d x)
    | N (g, e, d) when e > x -> N(supprime g x, e, d)
    | N (g, e, d) -> let rec aux g d = match d with
        |V -> g
        |N(g', e, d') -> N(aux g d', e, d)
    in aux g d;;

let rec supprime_min a = match a with
    |V -> failwith "arbre vide"
    |N(g, e, d) -> let mi, g' = supprime_min g in mi, N(g', e, d);;



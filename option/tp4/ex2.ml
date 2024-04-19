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

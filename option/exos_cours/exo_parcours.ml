type 'a t = V | N of 'a * 'a t * 'a t

let a = N('A', N('B', N('D', V, V), N('E', N('H', N('I', V, V), N('J', V, V)), N('K', V, V))), N('C', N('F', V, V), N('G', V, V)))

let rec prefixe a = match a with
    |V -> []
    |N(e, l, r) -> e::((prefixe l)@(prefixe r));;

let prefixe_efficace a = let rec aux l r = match l, r with
    |V, [] -> []
    |V, h::t -> aux h t
    |N(e, l', r'), _ -> e::aux l' (r'::r)
    in aux a [];;

let rec infixe a = match a with
    |V -> []
    |N(e, l, r) -> (infixe l)@e::infixe r;;

let infixe_efficace a = let rec aux l r = match l, r with
    |V, [] -> []
    |V, h::t -> aux h t
    |N(e, l', r'), _ -> if l' = V then e::aux r' r else aux l' (l::r'::r)
    in aux a [];;(*TODO*)

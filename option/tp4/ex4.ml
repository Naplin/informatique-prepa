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



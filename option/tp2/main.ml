let moyenne l = if l = [] then 0. else
        let rec aux l a b = match l with
                | [] -> a/.(float_of_int b)
                | h::t -> aux t (a+.h) (b+1)
        in aux l 0. 0;;

let rec concatener l1 l2 = match l1 with
        | [] -> l2
        | h::t -> h::concatener t l2;;

let rec miroir l = match l with
        | [] -> []
        | h::t -> concatener (miroir t) [h];;

let miroir_efficace l = let rec aux l a = match l with
        | [] -> a
        | h::t -> aux t (h::a)
        in aux l [];;

miroir_efficace [1; 2; 3; 4];;

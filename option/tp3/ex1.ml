let rec vectoriser f l = match l with
        | [] -> []
        | h::t -> f h::vectoriser f t;;

let liste_est_pair l = vectoriser (fun x -> x mod 2 = 0) l;;

let rec select c l = match l with
        | [] -> []
        | h::t -> if c h then h::select c t else select c t;;

let rec pour_tout c l = match l with
        | [] -> true
        | h::t -> if c h then pour_tout c t else false;;

let rec existe c l = match l with
        | [] -> false
        | h::t -> if c h then true  else existe c l;;

let rec compose f x = match f with
        | [] -> x
        | h::t -> h (compose t x);;

let est_sans_doublons_triee l = match l with
        | [] | [_] -> true
        | h::t -> let rec aux l a = match l with
                | [] -> true
                | h'::t' -> if h' = a then false else aux t' h'
        in aux t h;;

let supprime_doublons_triee l = match l with
        | [] -> []
        | h::t -> let rec aux l a = match l with 
                | [] -> []
                | h'::t' -> if h' = a then aux t' h' else h'::aux t' h'
        in h::aux t h;;

let rec est_sans_doublons l = match l with
        | [] -> true
        | h::t -> if List.mem h t then false else est_sans_doublons t;;

let rec supprime_doublons l = match l with
        | [] -> []
        | h::t -> let rec aux lp l = match l with
                | [] -> []
                | h::t -> if List.mem h lp then aux (h::lp) t else h::aux (h::lp) t
        in h::aux [h] t;;

let rec encode l = match l with
        | [] -> []
        | h::t -> match encode t with
                | [] -> [(1, h)]
                | (k, x)::t' -> if h = x then (k+1, x)::t' else (1, h)::(k, x)::t';;

let rec decode l = match l with
        | [] -> []
        | (k, x)::t -> if k = 0 then decode t else x::decode((k-1, x)::t);;



(* Ex1 *)

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

(* Ex2 *)

type 'a arbre =
        | Vide
        | Noeud of 'a arbre * 'a * 'a arbre;;

let rec nombre_feuilles arbre = match arbre with
        | Vide -> 0
        | Noeud(Vide, _, Vide) -> 1
        | Noeud(l, _, r) -> nombre_feuilles l + nombre_feuilles r;;

let a = Noeud(Noeud(Noeud(Vide, 5, Vide), 12, Noeud(Vide, 8, Vide)), 25, Noeud(Noeud(Noeud(Vide, 16, Vide), 52, Noeud(Vide, 27, Vide)), 2, Noeud(Vide, 10, Vide)));;

let rec prefixe arbre = match arbre with
        | Vide -> []
        | Noeud(Vide, x, Vide) -> [x]
        | Noeud(l, x, r) -> x::prefixe l@prefixe r;;

(* Ex3 *)

type couleur = Carreau | Pique | Trefle | Coeur;;
type tete = Valet | Dame | Roi;;
type valeur = Tete of tete | Nombre of int;;
type carte = {co : couleur; va : valeur};;

let duel c1 c2 = if c1.va = c2.va then 0 else match c1, c2 with
        | {va = Nombre v1}, {va = Nombre v2} -> if v1 > v2 then -1 else 1
        | {va = Tete _}, {va = Nombre _} | {va = Tete Roi}, _ | {va = Tete Dame}, {va = Tete Valet} -> -1
        | _ -> 1;;

let c1 = {co = Pique; va = Tete Dame};;
let c2 = {co = Trefle; va = Tete Valet};;

duel c1 c2;;

let partie l1 l2 = let rec aux l1 l2 a b = match l1, l2 with
        | [], [] -> b-a
        | h::t, h'::t' -> (match duel h h' with
                | 0 -> aux t t' a b
                | -1 -> aux t t' (a+1) b
                | _ -> aux t t' a (b+1))
        | _ -> 0
        in aux l1 l2 0 0;;

let l1 = [c1; c2];;
let l2 = [c2; c2];;

partie l1 l2;;

let deck = let rec aux c k = match k with
        | 13 -> [{co = c; va = Tete Roi}]
        | 12 -> {co = c; va = Tete Dame}::aux c (k+1)
        | 11 -> {co = c; va = Tete Valet}::aux c (k+1)
        | x -> {co = c; va = Nombre x}::aux c (x+1)
        in (aux Pique 1)@(aux Trefle 1)@(aux Coeur 1)@(aux Carreau 1);;

let shuffle d =
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond;;

let rec taille_liste l = match l with
        | [] -> 0
        | h::t -> 1 + taille_liste t;;

let split l = let rec aux l l1 l2 = match l with
        | [] -> l1, l2
        | h::t -> aux t (h::l2) l1
        in aux l [] [];;

let random_deck = shuffle deck;;
let deck1, deck2 = split random_deck;;

let partie_bataille deck1 deck2 = let rec aux d1 d2 a b = match d1, d2 with
        | [], _ | _, [] -> b-a
        | h1::t1, h2::t2 -> if a > 52 || b > 52 then 0 else match duel h1 h2 with
                | -1 -> aux (d1@[h2]) t2 (a+1) b
                | 1 -> aux t1 (d2@[h1]) a (b+1)
                | _ -> aux (t1@[h1]) (t2@[h2]) (a+1) (b+1)
        in aux deck1 deck2 0 0;;




let est_premier k = let rec aux k q = match q with
        | 1 -> true
        | x -> if k mod q = 0 then false else aux k (q-1)
        in aux k (int_of_float(sqrt(float_of_int k)));;

let decompo k = if est_premier k then [k] else let x = k/2 in
        let rec aux k i = match i with
                | -1 -> []
                | _ -> let j = x-i in if est_premier j && k mod j = 0 then j::aux (k/j) i else aux k (i-1)
        in aux k (x-2);;

let supprime_doublons l = match l with
        | [] -> []
        | h::t -> let rec aux lp l = match l with
                | [] -> []
                | h::t -> if List.mem h lp then aux (h::lp) t else h::aux (h::lp) t
        in h::aux [h] t;;

let produit_liste l = let rec aux l p = match l with 
        | [] -> p
        | h::t -> aux t (p*h)
        in aux l 1;;

let rec inter l1 l2 = match l1 with
        | [] -> []
        | h::t -> if List.mem h l2 then h::inter t l2 else inter t l2;;

let pgcd a b = produit_liste (supprime_doublons(inter (decompo(a)) (decompo(b))));;

(* --------------------------------------------- *)

type quotient = {num : int; den : int};;

let qsomme q1 q2 = if q1.den = q2.den then
        {num = q1.num + q2.num; den = q1.den} else
        {num = q1.num * q2.den + q2.num * q1.den; den = q1.den * q2.den};;

let qproduit q1 q2 = {num = q1.num * q2.num; den = q1.den * q2.den};;

let qdiff q1 q2 = qsomme q1 (qproduit {num = -1; den = 1} q2);;

let qdiv q1 q2 = qproduit q1 {num = q2.den; den = q2.num};;

(* --------------------------------------------- *)

type complexe = {re : float; im : float};;

let zsomme z1 z2 = {re = z1.re +. z2.re; im = z1.im +. z2.im};;

let ext_zproduit lambda z = {re = lambda *. z.re; im = lambda *. z.im};;
let zproduit z1 z2 = {re = z1.re *. z2.re -. z1.im *. z2.im; im = z1.re *. z2.im +. z1.im *. z2.re};;

let zdiff z1 z2 = zsomme z1 (ext_zproduit (-1.) z2);;

let zconj z = {re = z.re; im = -1.*.z.im};;
let zquot z1 z2 = ext_zproduit (1./.((z2.re)**2. +. (z2.im)**2.)) (zproduit z1 (zconj z2));; 
(*
a + ib
c + id
prod : ac - bd + i(ad + cb)
quot : *)

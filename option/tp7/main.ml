(* Ex1 *)

type complexe = {mutable re: float; mutable im: float}

let partie_reelle z = z.re;;
let partie_imaginaire z = z.im;;

let conjugue z = z.im <- -1. *. z.im; ;;
let reinitialiser z = z.re <- 0.; z.im <- 0.; ;;

let norme z = sqrt (z.re**2. +. z.im**2.);;

let incremente z = z.re <- z.re +. 1.; ;;

let multiplier a b = let re = (a.re *. b.re -. a.im *. b.im) in let im = (a.re *. b.im +. a.im *. b.re)
    in a.re <- re;
    a.im <- im; ;;

(* Ex2 *)

let f n = print_int n; print_newline (); ;;
let g n x = print_char '(';print_int n;print_char ',';print_float x;print_char ')'; print_newline ();;
let affiche_liste_entiers f l = List.iter f l;;

(* Ex3 *)

let affiche_ref_entier x = print_int (!x)

let echange_ref a b = let tmp = !b in b := !a; a := tmp

let creer_ref x = let y = ref x in y := x + 1; ;;

let incremente_entier x = x := !x + 1; ;;

let modifie_premier_element r x = let _, b = !r in r := (x, b); ;;

(* Ex4 *)

let somme_entiers n = let s = ref 0 in for i = 1 to n do
    s := !s + i;
    print_int (!s);
    print_newline ();
done;
!s;;

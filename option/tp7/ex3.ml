let affiche_ref_entier x = print_int (!x)

let echange_ref a b = let tmp = !b in b := !a; a := tmp

let creer_ref x = let y = ref x in y := x + 1; ;;

let incremente_entier x = x := !x + 1; ;;

let modifie_premier_element r x = let _, b = !r in r := (x, b); ;;



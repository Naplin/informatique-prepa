let f n = print_int n; print_newline (); ;;
let g n x = print_char '(';print_int n;print_char ',';print_float x;print_char ')'; print_newline ();;
let affiche_liste_entiers f l = List.iter f l;;



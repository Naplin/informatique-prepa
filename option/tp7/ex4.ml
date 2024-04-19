let somme_entiers n = let s = ref 0 in for i = 1 to n do
    s := !s + i;
    print_int (!s);
    print_newline ();
done;
!s;;



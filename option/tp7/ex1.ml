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

type 'a tas_simple = 
    { mutable taille : int; tab : 'a array };;

(* Ex1 - Tas-min *)
let tas = { taille = 6; tab = [|1; 2; 6; 5; 4; 6; 0; 0; 0|] }

let creer_tas taille_max element = { taille = 0; tab = Array.make taille_max element };;

let est_vide tas = tas.taille = 0;;

let echange tableau i j = let temp = tableau.(i) in
    tableau.(i) <- tableau.(j);
    tableau.(j) <- temp ;;

let pere i = (i-1)/2;;

let fils_gauche i = 2*i + 1;;

let fils_droit i = 2*i + 2;;

let rec percoler_haut tas i = let p = pere i in if tas.tab.(i) >= tas.tab.(p) || i = 0 then () else
    begin
        echange tas.tab i p;
        percoler_haut tas p
    end;;

let ajouter tas x = if tas.taille = Array.length tas.tab then failwith "tas plein" else
    begin
        tas.tab.(tas.taille) <- x;
        percoler_haut tas tas.taille;
        tas.taille <- tas.taille + 1
    end;;

let min2 tas i j = if tas.tab.(i) < tas.tab.(j) then i else j;;

let argmin3 tas i = let g = fils_gauche i in let d = fils_droit i in
    if g >= tas.taille then i else if d = tas.taille then min2 tas i g else min2 tas i (min2 tas g d);;

let rec percoler_bas tas i = let j = argmin3 tas i in if tas.tab.(i) <= tas.tab.(j) then () else
    begin
        echange tas.tab i j;
        percoler_bas tas j
    end;;

let rec retirer_min tas = echange tas.tab 0 (tas.taille - 1);
    tas.taille <- tas.taille - 1;
    percoler_bas tas 0;
    tas.tab.(tas.taille) ;;

(* Ex2 - Graphes, Dijkstra *)
type graphe = (int * int) list array

let exemple = 
[| [(1, 10); (3, 5)];
    [(2, 1); (3, 2)];
    [(4, 4)];
    [(1, 3); (2, 9); (4, 2)];
    [(0, 7); (2, 6)] |]

let rec longueur_liste l = match l with
    |[] -> 0
    |h::t -> 1 + longueur_liste t;;

let nombre_sommets graphe = Array.length graphe;;

let nombre_aretes graphe = let s = ref 0 in for i = 0 to nombre_sommets graphe
    do
        s := !s + longueur_liste graphe.(i)
    done;
    !s;;

let initialiser_file g s = { taille = 1; tab = Array.make (nombre_sommets g) (0, s) };;

let dijkstra g s = let file = initialiser_file g s in let dist = Array.make (nombre_sommets g) max_int in 
    dist.(s) <- 0;
    while not (est_vide file) do
        let _, x = retirer_min file in let rec aux l = match l with
            |[] -> ()
            |(y, py)::t -> if dist.(x) + py < dist.(y) then
                begin
                    dist.(y) <- dist.(x) + py;
                    ajouter file (dist.(y), y)
                end;
                aux t
        in aux g.(x);
    done;
    dist;;

(* PREAMBULE *)
let rec get_problem_data file =
  try Scanf.sscanf (input_line file) "%d %d" (fun n p -> (n, p))
  with Scanf.Scan_failure _ ->
    failwith "Erreur de format de ligne 1"

type graphe = (int * int) list array
(* g.(i) est la liste des couples (successeur, poids de l'arc) *)

let generate_graph (f : in_channel) (nb_sommets : int) : graphe =
  let g = Array.make nb_sommets [] in
  let aux () =
    let ligne = input_line f in
    try
      let x, y, d =
        Scanf.sscanf ligne "%d %d %d" (fun x y d -> (x, y, d))
      in
      g.(x) <- (y, d) :: g.(x)
    with Scanf.Scan_failure _ ->
      failwith "Erreur de format de ligne dans le graphe"
  in
  begin
    try
      while true do
        aux ()
      done
    with End_of_file -> ()
  end ;
  g

let generate_positions (f : in_channel) (nb_sommets : int) =
  let g = Array.make nb_sommets (0, 0) in
  let aux () =
    let ligne = input_line f in
    try
      let k, x, y =
        Scanf.sscanf ligne "%d %d %d" (fun k x y -> (k, x, y))
      in
      g.(k) <- (x, y)
    with Scanf.Scan_failure _ ->
      failwith "Erreur de format de ligne dans le graphe"
  in
  begin
    try
      while true do
        aux ()
      done
    with End_of_file -> ()
  end ;
  g


let lire_graphe filename =
  let f = open_in filename in
  let n, _ = get_problem_data f in
  let x = generate_graph f n in
  close_in f ; x

let time f x =
  let t = Sys.time () in
  let res = f x in
  let t' = Sys.time () in
  Printf.printf "Temps d'exécution : %fs\n" (t' -. t) ;
  res;;

let lire_positions filename =
    let f = open_in filename in
    let n = try Scanf.sscanf (input_line f) "%d" (fun n -> n)
        with Scanf.Scan_failure _ -> failwith "Erreur de format de ligne 1" in
    let x = generate_positions f n in
    close_in f ; x;;

let plus_court_chemin g a b = let file = initialiser_file g a in let dist = Array.make (nombre_sommets g) max_int in 
    dist.(a) <- 0;
    let pred = Array.make (nombre_sommets g) [] in let aux0 () =
    try
        begin
            while not (est_vide file) do
                let px, x = retirer_min file in if x = b then raise Exit else let rec aux l = match l with
                    |[] -> ()
                    |(y, py)::t -> if dist.(x) + py < dist.(y) then
                        begin
                            pred.(y) <- x::pred.(x);
                            dist.(y) <- dist.(x) + py;
                            ajouter file (dist.(y), y)
                        end;
                        aux t
                in aux g.(x)
            done
        end
    with Exit -> () in aux0 ();
    List.rev pred.(b);;

let distance_vol_oiseau positions a b = let x1, y1 = positions.(a) in let x2, y2 = positions.(b) in int_of_float (sqrt (((float_of_int x1)-.(float_of_int x2))**2. +. ((float_of_int y1)-.(float_of_int y2))**2.));;

let a_star g pos a b = let file = initialiser_file g a in let dist = Array.make (nombre_sommets g) max_int in 
    dist.(a) <- 0;
    let pred = Array.make (nombre_sommets g) [] in let aux0 () =
    try
        begin
            while not (est_vide file) do
                let px, x = retirer_min file in if x = b then raise Exit else let rec aux l = match l with
                    |[] -> ()
                    |(y, py)::t -> if dist.(x) + py < dist.(y) then
                        begin
                            pred.(y) <- x::pred.(x);
                            dist.(y) <- dist.(x) + py;
                            ajouter file (dist.(y) + distance_vol_oiseau pos y b, y)
                        end;
                        aux t
                in aux g.(x)
            done
        end
    with Exit -> () in aux0 ();
    List.rev pred.(b);;

(* Ex3 *)

let diminuer_priorite (tas: ('a * 'b) tas_simple) i reduction =
    let _, b = tas.tab.(i) in
    tas.tab.(i) <- (reduction, b);
    percoler_haut tas i ;;

type 'a tas_complet = { 
    mutable taille : int;
    priorites : 'a array;
    position : int array;
    cle : int array
}

let creer_tas_complet capacite = { 
    taille = 0;
    priorites = Array.make capacite 0;
    position = Array.make capacite 0;
    cle = Array.make capacite 0;
}

let est_vide_complet tas = tas.taille = 0;;

let echange_complet tas i j =
    let pos_i = tas.position.(i) in
    let pos_j = tas.position.(j) in
    let prio_i = tas.priorites.(pos_i) in
    let cle_i = tas.cle.(pos_i) in
    tas.position.(i) <- pos_j;
    tas.position.(j) <- pos_i;
    tas.priorites.(pos_i) <- tas.priorites.(pos_j);
    tas.priorites.(pos_j) <- prio_i;
    tas.cle.(pos_i) <- tas.cle.(pos_j);
    tas.cle.(pos_j) <- cle_i;;

let rec percoler_haut_complet tas x =
    let i = tas.position.(x) in
    let p = pere i in
    if tas.priorites.(p) <= tas.priorites.(i) || i = 0 then
        ()
    else
        begin
            echange_complet tas x tas.cle.(p);
            percoler_haut_complet tas x
        end;;

let min2_complet tas i j =
    if tas.priorites.(i) < tas.priorites.(j) then
        i
    else
        j;;

let argmin3_complet tas x =
    let i = tas.position.(x) in
    let g = fils_gauche i in
    let d = fils_droit i in
    if g >= tas.taille then
        x
    else if d = tas.taille then
        tas.cle.(min2_complet tas i g)
    else
        tas.cle.(min2_complet tas i (min2_complet tas g d));;

let rec percoler_bas_complet tas x =
    let mi = argmin3_complet tas x in
    let i = tas.position.(x) in
    let j = tas.position.(mi) in
    if tas.priorites.(j) >= tas.priorites.(i) then
        ()
    else
        begin
            echange_complet tas x mi;
            percoler_bas_complet tas x
        end;;

let ajouter_complet tas (p, x) =
    if tas.taille = Array.length tas.cle then
        failwith "tas plein"
    else
        if tas.position.(x) = -1 then
            begin
                tas.position.(x) <- tas.taille;
                tas.cle.(tas.taille) <- x;
                tas.priorites.(tas.taille) <- p;
                percoler_haut_complet tas x;
                tas.taille <- tas.taille + 1;
            end
        else
            if tas.priorites.(tas.position.(x)) > p then
                begin
                    tas.priorites.(tas.position.(x)) <- p;
                    percoler_haut_complet tas x
                end;;

let retirer_min_complet tas =
    echange_complet tas (tas.cle.(0)) (tas.cle.(tas.taille - 1));
    tas.taille <- tas.taille - 1;
    percoler_bas_complet tas (tas.cle.(0));
    tas.cle.(tas.taille);;

let initialiser_file_v2 g s = let file = creer_tas_complet (nombre_sommets g) in
    ajouter_complet file (0, s);
    file;;

let dijkstra_v2 g s = let file = initialiser_file_v2 g s in let dist = Array.make (nombre_sommets g) max_int in 
    dist.(s) <- 0;
    while not (est_vide_complet file) do
        let x = retirer_min_complet file in let rec aux l = match l with
            |[] -> ()
            |(y, py)::t -> if dist.(x) + py < dist.(y) then
                begin
                    dist.(y) <- dist.(x) + py;
                    ajouter_complet file (dist.(y), y)
                end;
                aux t
        in aux g.(x);
    done;
    dist;;

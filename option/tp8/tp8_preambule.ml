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
  res

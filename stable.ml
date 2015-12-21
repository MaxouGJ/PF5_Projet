open Automaton;;
open Logic;;
open Str;;

(*Exception lancée lorsque sortie renvoie SAT mais qu'il n'y a pas d'affectaction derrière*)
exception Sat_exception;;
(*Exception lancée lorsque sortie renvoie UNSAT*)
exception Unsat_exception;;
(*Exception lancée lorsque minisat échoue*)
exception Minisat_exception;;

(*Ajoute s à la fin d'entree.dimacs*)
let add_dimacs s =
  let fic = open_out_gen [Open_append] 0 "entree.dimacs" in
  output_string fic ("\n"^s);
  close_out fic;
;;

(*Inverse le sens de la liste l et donne la négation de chaque variable*)
let list_neg l =
  let rec list_neg_rec l s =
      match l with
      |[] | [_] -> (s ^ " 0")
      |h :: t -> if String.get h 0 = '-' then list_neg_rec t (s ^" "^ (String.sub h 1 ((String.length h) -1)))
	  else list_neg_rec t (s ^ " -" ^h)
  in list_neg_rec l ""	  
;;

(*Récupère les variables du résultat de minisat et les ajoute dans entree.dimacs*)
let recup_var f =
  let line =
    try
      Some (input_line f)
    with
      End_of_file -> None
  in match line with
  |Some x -> let l = Str.split (Str.regexp(" ")) x in add_dimacs (list_neg l); l
  |None -> print_string "La formule est satisfaisable mais minisat n'a réussi aucune affectation"; raise Sat_exception 
;;

(*Affiche l'automate stable de dimension d comportant les valeurs des variables dans l*)
let affiche_stable l d = 
  let g = Array.make_matrix d d Dead in
  let rec constr l g i j d=
    match l with
    |[] | [_] -> () (*Pour ne pas traiter le 0 de fin*)
    |h :: t -> if String.get h 0 != '-' then  g.(i).(j) <- Alive;
      if j= (d-1) then constr t g (i+1) 0 d
      else constr t g i (j+1) d
  in constr l g 0 0 d;
  show_generation g;
;;

(*Parse le fichier entree.dimacs*)
let parse_dimacs d =
  let fic = open_in "sortie" in
  let line = 
    try
      Some(input_line fic)
    with
      End_of_file -> None
  in match line with
  |Some ("SAT") -> affiche_stable (recup_var fic) d;
  |Some ("UNSAT") -> print_string "Aucune génération stable trouvée.\n"; raise Unsat_exception
  |Some (_) -> print_string "Erreur lors de l'exécution de minisat.\n"; raise Minisat_exception
  |None -> print_string "Erreur lors de l'exécution de minisat.\n"; raise Minisat_exception
;;

(*Boucle répétée pour avoir autant de formule stable que demandé*)
let rec boucle d =
  if (Sys.command "minisat entree.dimacs sortie") = 1
  then print_string "Echec de minisat.\n";
  let dimacs = 
    try Some(parse_dimacs d)
    with
    |Unsat_exception -> None
    |Minisat_exception -> None
  in match dimacs with
  |None -> ()
  |_ -> 
     print_string "Presser la touche :
\n\t-q pour quitter
\n\t-entrée pour afficher une autre génération stable\n";
    match read_line() with
    |"q" -> print_string "Au revoir.\n"
    |_ -> boucle d
;;

(*Permet de savoir si l'automate a de taille d a au moins une configuration stable*)
let show_stable (a:automaton) d =
  create_dimacs (stables a d);
  boucle d;
;;

open Automaton;;
open Logic

exception Sat_exception;;
exception Unsat_exception;;

(*Ajoute s à la fin d'entree.dimacs*)
let add_dimacs s =
  let fic = open_out_gen [Open_append] 0 "entree.dimacs" in
  output_string fic ("\n"^s);
  close_out fic;
;;

(*Renvoie une liste de toutes les variables du string s*)
let split s =
  let rec aux s v l =
    if String.length s = 1 then l (*Pour ne pas récupérer le 0 de fin on s'arrête un caractère avant*)
      else
	match s with
	|" " -> aux (String.sub s 1 ((String.length s)-1)) "" (v::l)
	|x -> aux (String.sub s 1 ((String.length s)-1)) (v^x)  l
  in List.rev (aux s "" [])
;;

(*Récupère les variables du résultat de minisat et les ajoute dans entree.dimacs*)
let recup_var f =
  let line =
    try
      Some (input_line f)
    with
      End_of_file -> None
  in match line with
  |Some x -> add_dimacs x; split x
  |None -> raise Sat_exception 
;;

(*Affiche l'automate stable de dimension d comportant les valeurs des cariables dans l*)
let affiche_stable l d = 
  print_string "\n--------------------\n";
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
  |Some ("UNSAT") -> raise Unsat_exception
  |Some ("SAT") -> affiche_stable (recup_var fic) d;
  |Some (_) -> raise Format_non_standard
  |None -> raise Format_non_standard
;;

let rec boucle d =
  match (parse_dimacs d)  with
  (*|Unsat_exception -> print_string "Il n'y a plus de génération stable à afficher.\n"
    |Format_non_standard -> print_string "Erreur lors de l'execution de minisat.\n"*)
  |_ -> print_string "Presser :\n\t-q pour quitter\n\t-entrée pour afficher une autre génération stable\n";
    begin
      match read_line() with
      |"" -> boucle d
      |"q" -> print_string "Au revoir.\n"
      |_ -> print_string "Vous n'avez pas le droit à l'erreur bye.\n"
    end
;;

(*Permet de savoir si l'automate a de taille d a au moins une configuration stable*)
let show_stable (a:automaton) d =
  create_dimacs (stables a d);
  if (Sys.command "minisat entree.dimacs sortie") = 1
  then print_string "Echec de minisat";
  match (parse_dimacs d) with
  (*|Unsat_exception -> print_string "Il n'y a pas de configuration stable pour cette automate.\n"
    |Format_non_standard -> "Erreur lors de l'execution de minisat.\n"*)
  |_ -> print_string "Presser :\n\t-q pour quitter\n\t-entrée pour afficher une autre génération stable\n";
    begin
      match read_line() with
      |"" -> boucle d
      |"q" -> print_string "Au revoir.\n"
      |_ -> print_string "Vous n'avez pas le droit à l'erreur bye.\n"
    end
;;

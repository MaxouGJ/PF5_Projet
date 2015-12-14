open Automaton;;
open Logic;;

(*Permet d'afficher les génératon au fur et a mesure du temps*)
let rec tour a g =
  print_string "Pressez la touche Entrer pour faire avancer d'une génération ou q pour quitter\n";
  match read_line() with
  |"" -> let newG = next_generation a g in
	 show_generation newG;
	 tour a newG
  |"q" -> print_string "Au revoir\n"
  |_ -> print_string "Mauvaise touche, désolé pas le droit à l'erreur...\n"
;;

(*Permet de lancer toute la partie logique*)
let logique a i =
  dimacs (stables a i);
  if (Sys.command "minisat entree.dimacs sortie") = 1
  then print_string "Echec de minisat"
;;

let rec read_fic () =
  try
    print_string "Entrer un nom de fichier au format standard\n";
    let p = parse (read_line()) in
    match p with
    |(i, a, g) -> show_generation g;
      print_string "Appuyer sur l pour lancer la partie logique, appuyer sur entrée pour afficher la prochaine génération\n";
      begin match (read_line()) with
      |"l" -> logique a i
      |_ -> tour a g
      end
  with
  |Format_non_standard -> print_string "Impossible de lire le fichier, il n'est pas au format standard\n";
    read_fic ();
;;

read_fic();;

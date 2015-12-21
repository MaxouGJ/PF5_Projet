open Automaton;;
open Logic;;
open Stable;;

(*Permet de lancer toute la partie logique*)
let logique a i =
  show_stable a i;
;;

(*Permet d'afficher les génératon au fur et a mesure du temps*)
let rec tour a g i=
  let newG = next_generation a g in show_generation newG;
  print_string "Pressez la touche :
\n\t- Entrée pour faire avancer d'une génération
\n\t- s pour afficher une génération stable
\n\t- q pour quitter\n";
  match read_line() with
  |"" -> tour a newG i
  |"q" -> print_string "Au revoir.\n"
  |"s" -> logique a i
  |_ -> print_string "Touche incomprise.\n"; tour a g i
;;

let rec read_fic () =
  try
    print_string "Entrer un nom de fichier au format standard.\n";
    let p = parse (read_line()) in
    match p with
    |(i, a, g) -> show_generation g;
      print_string "Pressez la touche :
\n\t- s pour afficher une génération stable
\n\t- entrée pour afficher la prochaine génération
\n\t- q pour quitter le programme\n";
      begin match (read_line()) with
      |"s" -> logique a i
      |"q" -> print_string "Au revoir.\n"
      |_ -> tour a g i
      end
  with
  |Format_non_standard -> print_string "Impossible de lire le fichier, il n'est pas au format standard.\n";
  |Fichier_introuvable -> print_string "Vérifier le nom et l'emplacement de votre fichier.\n"; 
    read_fic ();
;;

read_fic();;

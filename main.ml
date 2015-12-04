open Automaton;;

let rec tour a g =
  print_string "Pressez la touche Entrer pour faire avancer d'une génération ou q pour quitter\n";
  match read_line() with
  |"" -> let newG = next_generation a g in
	 show_generation newG;
	 tour a newG
  |"q" -> print_string "Au revoir\n"
  |_ -> print_string "Mauvaise touche, désolé pas le droit à l'erreur...\n"
;;
  
let rec read_fic () =
  try
    print_string "Entrer un nom de fichier au format standard\n";
    let p = parse (read_line()) in
    match p with
    |(i, a, g) -> show_generation g; tour a g
  with
  |Format_non_standard -> print_string "Impossible de lire le fichier\n";
    read_fic ();
;;

read_fic();;

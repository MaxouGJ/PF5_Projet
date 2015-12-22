(*Les états possibles d'une cellule*)
type state = Alive | Dead;;
(*Une règle de l'automate*) (*Nord, Est, Sud, Ouest, Case*)
type rule = (state * state * state * state * state);;
(*Une génération*)
type generation = state array array;;
(*Un automate*)
type automaton = rule list;;

(*Exception lancée lorsque le fichier n'est pas au format standard*)
exception Format_non_standard;;
(*Exception lancée lorsque lefichier n'est pas trouvé*)
exception Fichier_introuvable;;

(*Fonctions auxiliaires de parse*)

(*Lit l'entier du fichier*)
let read_int f =
  let n =
    try
      Some(input_line f)
    with
      End_of_file -> None
  in match n with
  |Some(x) -> let s =
		try
		  Some(int_of_string x)
		with
		  int_of_string -> None
	      in begin
		match s with
		|Some(int) -> int
		|_ -> print_string "Le premier caractère du fichier n'est pas un entier.\n"; raise Format_non_standard
	      end 
  |_ -> print_string "Le fichier donné en argument est vide.\n"; raise Format_non_standard
;;

(*Lit la génération zéro du fichier*)
let read_generation f n =
  let g = Array.make_matrix n n Dead in
  for l=0 to n-1 do 
    let line =
      try
	Some(input_line f)
      with
	End_of_file -> None
    in match line with
    |None -> print_string "Impossible de lire la génération, vérifier le nombre de lignes de celle-ci.\n"; raise Format_non_standard
    |Some(s) -> if String.length s > n
      then (print_string "La génération ne correspond pas à la taille donnée en en-tête de fichier.\n"; raise Format_non_standard)
      else
	for c=0 to n-1 do
	  if String.get s c = 'A' then g.(l).(c) <- Alive;
	done;
    done;
  g;
;;

(*Convertit un caractère en state*)
let get_rule s =
  match s with
  |'A' -> Alive
  |'D' -> Dead
  |_ -> print_string "Un caractère de la génération est différent de A ou D.\n"; raise Format_non_standard
;;

(*Convertit en règle le string r*)
let str_to_rule r =
  if String.length r <> 5 then (print_string "Une des règles ne comporte pas 5 caractères.\n"; raise Format_non_standard)
  else ((get_rule (String.get r 0)),
	(get_rule (String.get r 1)),
	(get_rule (String.get r 2)),
	(get_rule (String.get r 3)),
	(get_rule (String.get r 4)))
;;

(*Lit les règles du fichier*)
let read_rules_and_gen f n =
  let rec aux f l =
    let line =
      try
	Some(input_line f)
      with
	End_of_file -> None
    in match line with
    |Some(x) -> if x = "GenerationZero" then (n, l,(read_generation f n))
      else aux f ((str_to_rule x) :: l)
    |None -> print_string "Problème lors de la lecture des règles.\n"; raise Format_non_standard
  in aux f []
;;

(*Fin des fonctions auxiliaires*)

(*Lit le fichier f au format standard, initialise un automate avec la génération spécifiée*)
let parse fic =
  let f =
    try
      open_in fic
    with _ ->
      (print_string "Fichier introuvable.\n"; raise Fichier_introuvable)
  in
  let n = read_int f in
  let regle =
    try
      Some(input_line f)
    with
      End_of_file -> None
  in match regle with
  |Some(x) -> if x = "Regles" then (read_rules_and_gen f n)
    else (print_string "Impossible de lire Regles.\n"; raise Format_non_standard)
  |None -> print_string "Impossible de lire Regles.\n"; raise Format_non_standard
;;

(*Affiche une grille g de taille n*)
let show_generation g =
  let n = Array.length g in
  for i = 0 to n-1 do
    for k = 0 to n-1 do
      print_string("+---");
      if k = n-1 then print_string("+");
      if k = n-1 then print_newline();
    done;
    for j = 0 to n-1 do
      if g.(i).(j) = Alive
      then print_string("| A ")
      else print_string("| D ");
      if j = n-1 then print_string("|");
    done;
      print_newline();
  done;
  for k = 0 to n-1 do
    print_string("+---");
    if k = n-1 then print_string("+");
    if k = n-1 then print_newline();
  done
;;

(*Convertit une case et son entourage en rule*)
(*Nord, Est, Sud, Ouest, Case*)
let line_to_rule (g:generation) i j =
  let n = (Array.length g - 1) in
  if n = 0 then
    (g.(0).(0), g.(0).(0), g.(0).(0), g.(0).(0), g.(0).(0))
  else
    if i = 0 then
      if j = 0 then
	(g.(n).(j), g.(i).(j+1), g.(i+1).(j), g.(i).(n), g.(i).(j))
      else
	if j = n then
	  (g.(n).(j), g.(i).(0), g.(i+1).(j), g.(i).(j-1), g.(i).(j))
	else
	  (g.(n).(j), g.(i).(j+1), g.(i+1).(j), g.(i).(j-1), g.(i).(j))	
    else
      if i = n then
	if j = 0 then
	  (g.(i-1).(j), g.(i).(j+1), g.(0).(j), g.(i).(n), g.(i).(j))
	else
	  if j = n then
	    (g.(i-1).(j), g.(i).(0), g.(0).(j), g.(i).(j-1), g.(i).(j))
	  else
	    (g.(i-1).(j), g.(i).(j+1), g.(0).(j), g.(i).(j-1), g.(i).(j))
      else
	if j = 0 then
	  (g.(i-1).(j), g.(i).(j+1), g.(i+1).(j), g.(i).(n), g.(i).(j))
	else
	  if j = n then
	    (g.(i-1).(j), g.(i).(0), g.(i+1).(j), g.(i).(j-1), g.(i).(j))
	  else
	    (g.(i-1).(j), g.(i).(j+1), g.(i+1).(j), g.(i).(j-1), g.(i).(j))
;;

(*Fait évoluer la génération g selon la règle de l'automate a*)
let next_generation a g =
  let n = Array.length g in
  let newG = Array.make_matrix n n Dead in
  for i=0 to (n-1) do
    for j=0 to (n-1) do
      let rec check g i j l =
	match l with
	|[] -> ()
	|h :: t -> if (line_to_rule g i j) = h then newG.(i).(j) <- Alive
	  else check g i j t
      in check g i j a
    done;
  done;
  newG
;;

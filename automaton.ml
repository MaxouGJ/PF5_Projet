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

(*Fonctions auxiliaires de parse*)

(*Lit l'entier du fichier*)
let readInt f =
  let n =
    try
      Some(input_line f)
    with
      End_of_file -> None
  in match n with
  |Some(x) -> int_of_string x
  |_ -> raise Format_non_standard
;;

(*Lit la génération zéro du fichier*)
let readGeneration f n =
  let g = Array.make_matrix n n Dead in
  for l=0 to n-1 do 
    let line =
      try
	Some(input_line f)
      with
	End_of_file -> None
    in match line with
    |None -> raise Format_non_standard
    |Some(s) -> if String.length s > n then raise Format_non_standard
      else
	for c=0 to n-1 do
	  if String.get s c = 'A' then g.(l).(c) <-  Alive;
	done;
    done;
  g;
;;

(*Converti en règle le string r*)
let listRule r =
  let rec readR r l =
    if String.length r = 0 then l
    else let str = String.sub r 0 1 in
	 match str  with
	 |"A" -> readR (String.sub r 1 ((String.length r) -1)) (Alive :: l)
	 |"D" -> readR (String.sub r 1 ((String.length r) -1)) (Dead :: l)
	 |_ -> raise Format_non_standard
  in match readR r [] with
  |e::d::c::b::a::[] -> (a,b,c,d,e)
  |_ -> raise Format_non_standard
;;

(*Lit les règles du fichier*)
let readRulesAndGen f n =
  let rec aux f l =
    let line =
      try
	Some(input_line f)
      with
	End_of_file -> None
    in match line with
    |Some(x) -> if x = "GenerationZero" then (n, l,(readGeneration f n))
      else aux f ((listRule x) :: l)
    |None -> raise Format_non_standard
  in aux f []
;;

(*Fin des fonctions auxiliaires*)

(*Lit le fichier f au format standard, initialise un automate avec la génération spécifiée*)
let parse fic =
  let f = open_in fic in
  let n = readInt f in
  let regle =
    try
      Some(input_line f)
    with
      End_of_file -> None
  in match regle with
  |Some(x) -> if x = "Regles" then (readRulesAndGen f n)
    else raise Format_non_standard
  |None -> raise Format_non_standard
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
let lineToRule (g:generation) i j =
  let n = (Array.length g - 1) in
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
	(g.(i-1).(j), g.(i).(j+1), g.(n).(j), g.(i).(n), g.(i).(j))
      else
	if j = n then
	  (g.(i-1).(j), g.(i).(0), g.(n).(j), g.(i).(j-1), g.(i).(j))
	else
	  (g.(i-1).(j), g.(i).(j+1), g.(n).(j), g.(i).(j-1), g.(i).(j))
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
	|h :: t -> if (lineToRule g i j) = h then newG.(i).(j) <- Alive
	  else check g i j t
	in check g i j a
    done;
  done;
  newG
;;	

let getAutomaton f =
  match f with
  |(n, a, g) -> a
;;

let getGeneration f =
  match f with
  |(n, a, g) -> g
;;

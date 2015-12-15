open Automaton;;

(*Représente toutes les formules logiques*)
type formula = True
		|False
		|Var of int
		|And of formula * formula
		|Or of formula * formula
		|Neg of formula;;

exception Unsat_exception;; 

(*Règles de bases*)
let baseRules = ref [(Alive,Alive,Alive,Alive,Alive);
		 (Alive,Alive,Alive,Dead,Alive);
		 (Alive,Alive,Dead,Alive,Alive);
		 (Alive,Dead,Alive,Alive,Alive);
		 (Dead,Alive,Alive,Alive,Alive);
		 (Alive,Alive,Dead,Dead,Alive);
		 (Alive,Dead,Alive,Dead,Alive);
		 (Dead,Alive,Alive,Dead,Alive);
		 (Alive,Dead,Dead,Alive,Alive);
		 (Dead,Alive,Dead,Alive,Alive);
		 (Dead,Dead,Alive,Alive,Alive);
		 (Alive,Dead,Dead,Dead,Alive);
		 (Dead,Alive,Dead,Dead,Alive);
		 (Dead,Dead,Alive,Dead,Alive);
		 (Dead,Dead,Dead,Alive,Alive);
		 (Dead,Dead,Dead,Dead,Alive)]
;;

(*Retire les règles stables de l'automate des règles de base*)
let triBaseRules (l:automaton) =
  (*Copie de baseRules*)
  let br = ref !baseRules in
  (*Suppression des règles stables l*)
  let rec deleteRule l =
    match l with
    |[] -> !br
    |h :: t -> 
       (*parcours la copie de baseRules et supprime la règle h de br*)
       let rec aux headofrules list headofbr br=
	 match list with
	 |[] -> ()
	 |h :: t -> 
	    if h = headofrules then br := (headofbr@t)
	    else (aux headofrules t (h::headofbr) br)
       in aux h (!br) [] br;
       deleteRule t
  in deleteRule l
;;

(*Fonctions annexes*)
(*Retourne l'id de la case dans la position demandée*)
let getId d i j = (d*i+j+1);;
let getNorth d i j = if i = 0 then (d*(d-1)+j+1) else (d*(i-1)+j+1);;
let getSouth d i j = if i = (d-1) then (j+1) else (d*(i+1)+j+1);;
let getEast d i j = if j = (d-1) then (d*i+1) else (d*i+j+1+1);;
let getWest d i j = if j = 0 then (d*i+(d-1)+1) else (d*i+j-1+1);;

(*Retourne la négation de la variable en fonction de l'état de la case*)
let getNegVar (s:state) i =
  match s with
  |Alive -> Neg(Var(i))
  |Dead -> Var(i)
;;

(*Converti une règle en formule*)
let stables (a:automaton) d =
  let f = ref True in
  let clauses = ref 0 in
  for i=0 to d-1 do
    for j=0 to d-1 do 
      let rec rules r d i j f =
	match r with
	|[] -> !f
	|(n,e,s,o,c)::t ->
	   f := And(Or(getNegVar n (getNorth d i j),
		       Or(getNegVar e (getEast d i j),
			  Or(getNegVar s (getSouth d i j),
			     Or(getNegVar o (getWest d i j),
				getNegVar c (getId d i j))))),!f);
	  clauses := !clauses + 1;
	  rules t d i j f
      in f := And((rules (triBaseRules a) d i j (ref True)),!f);
    done;
  done;
  (!f, d, !clauses)
;;

(*Converti une formule en string à mettre dans le fichier dimacs*)
let rec formula_to_dimacs f =
  match f with
  |True|False -> ""
  |Var(x) -> (string_of_int x) ^ " "
  |Neg(x) -> "-" ^ (formula_to_dimacs x)
  |Or(x, y) -> (formula_to_dimacs x) ^ (formula_to_dimacs y)
  |And(x, y) ->
     begin match x with
     |True |False -> (formula_to_dimacs y)
     |_ -> 
	match y with
	|True |False -> (formula_to_dimacs x)
	|_ -> (formula_to_dimacs x) ^ "0\n" ^ (formula_to_dimacs y)
     end
;;(*Ajouter un 0 à la fin de l'appel à cette méthode*)

(*Converti une formule en fichier dimacs*)
let dimacs (f, d, c) =
  let fic = open_out "entree.dimacs" in
  output_string fic ("p cnf " ^ (string_of_int (d*d)) ^ " " ^ (string_of_int c) ^ "\n");
  output_string fic ((formula_to_dimacs f) ^ "0");
  close_out fic;    
;;

(*Parse le fichier entree.dimacs*)
let parse_dimacs () =
  let fic = open_in "sortie" in
  let line = 
    try
      Some(input_line fic)
    with
      End_of_file -> None
  in match line with
  |Some ("UNSAT") -> raise Unsat_exception
  |Some ("SAT") -> print_string "Sat"
  |Some (_) -> raise Format_non_standard
  |None -> raise Format_non_standard
;;

open Automaton;;

(*Représente toutes les formules logiques*)
type formula = True
		|False
		|Var of int
		|And of formula * formula
		|Or of formula * formula
		|Neg of formula;;

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

(*Vérifie si une règle est stable*)
let isStabeRule (r:rule) = match r with (n, e, s, o, c) -> c = Alive;;

let print_state (s:state) = if s = Alive then print_string "A" else print_string "D";;

let print_rules (r:rule) =
  match r with (a, b, c, d, e) ->
    print_string"[";
    print_state a;
    print_state b;
    print_state c;
    print_state d;
    print_state e;
    print_string"]"
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
(*Converti une formula en string*)
let rec string_of_formule = 
function
|True -> "Vrai"
|False -> "Faux"
|Var x -> string_of_int x
|And (x, y) -> "(" ^ (string_of_formule x) ^ " Et " ^ (string_of_formule y) ^ ")"
|Or (x, y) -> "(" ^ (string_of_formule x) ^ " Ou " ^ (string_of_formule y) ^ ")"
|Neg y -> " Neg " ^ "(" ^ (string_of_formule y) ^ ")"
;;

(*Retourne l'id de la case dans la position demandée*)
let getId d i j = (d*i+j+1);;
let getNorth d i j = if i = 0 then (d*(d-1)+j+1) else (d*(i-1)+j+1);;
let getSouth d i j = if i = (d-1) then (j+1) else (d*(i+1)+j+1);;
let getEast d i j = if j = (d-1) then (d*i+1) else (d*i+j+1+1);;
let getWest d i j = if j = 0 then (d*i+(d-1)+1) else (d*i+j-1+1);;

(*Retourne la variable en fonction de l'état de la case*)
let getVar (s:state) i =
  match s with
  |Alive -> Var(i)
  |Dead -> Neg(Var(i))
;;

(*Converti une règle en formule*)
let stables (a:automaton) d =
  let f = ref True in
  for i=0 to d do
    for j=0 to d do 
      let rec rules r d i j f =
	match r with
	|[] -> !f
	|(n,e,s,o,c)::t ->
	   f := And(Or(Neg(getVar n (getNorth d i j)),
		       Or(Neg(getVar e (getEast d i j)),
			  Or(Neg(getVar s (getSouth d i j)),
			     Or(Neg(getVar o (getWest d i j)),
				getVar c (getId d i j))))),!f);
	  rules t d i j f
      in f := And((rules (triBaseRules a) d i j (ref True)),!f);
    done;
  done;
  print_string (string_of_formule (!f));
  !f
;;



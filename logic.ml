open Automaton;;

(*Représente toutes les formules logiques*)
type formula = True
		|False
		|Var of int
		|And of formula * formula
		|Or of formula * formula
		|Neg of formula;;

(*Fonctions annexes*)

let rec string_of_formule = 
function
|True -> "Vrai"
|False -> "Faux"
|Var x -> string_of_int x
|And (x, y) -> "(" ^ (string_of_formule x) ^ " Et " ^ (string_of_formule y) ^ ")"
|Or (x, y) -> "(" ^ (string_of_formule x) ^ " Ou " ^ (string_of_formule y) ^ ")"
|Neg y -> " Neg " ^ "(" ^ (string_of_formule y) ^ ")"
;;

let rec desc_n f = 
match f with
|Neg(Neg x) -> desc_n x
|Neg(And(x, y)) -> Or((desc_n x),(desc_n y))
|Neg(Or(x, y)) -> And((desc_n x),(desc_n y))
|And(x, y) -> And((desc_n x),(desc_n y))
|Or(x, y) -> Or((desc_n x),(desc_n y))
|_ -> f;;

let rec desc_ou f = 
match f with 
|And(x, y) -> And((desc_ou x),(desc_ou y))
|Or(x, y) -> let g1 = (desc_ou x) and g2 = (desc_ou y) 
	     in match (g1, g2) with 
	     |And(a, b), _ ->
		And((desc_ou (Or(a, g2)),(desc_ou (Or(b, g2)))))
;;

let fnc f = desc_ou(desc_n f);;

let getId d i j = (d*i+j+1);;
let getNorth d i j = if i = 0 then (d*(d-1)+j+1) else (d*(i-1)+j+1);;
let getSouth d i j = if i = (d-1) then (j+1) else (d*(i+1)+j+1);;
let getEast d i j = if j = (d-1) then (d*i+1) else (d*i+j+1+1);;
let getWest d i j = if j = 0 then (d*i+(d-1)+1) else (d*i+j-1+1);;

let fold_and  l =
  let rec aux l f =
    match l with
    |[] -> f
    |h :: t -> aux t (And(h,f))
  in aux l True
;;

let fold_or  l =
  let rec aux l f =
    match l with
    |[] -> f
    |h :: t -> aux t (Or(h,f))
  in aux l False
;;

let rec print_list l =
  match l with
  |[] -> print_string "Vide"
  |h :: t ->print_string "a"; print_string(string_of_formule h); print_list t
;;

let getVar (s:state) i =
  match s with
  |Alive -> Var(i)
  |Dead -> Neg(Var(i))
;;

(*Converti une règle en formule*)
let stables (a:automaton) d =
  let list = ref [] in
  for i=0 to d do
    for j=0 to d do 
      let rec rules a i j d f =
	match a with
	|[] -> f
	|(n, e, s, o, c) :: t ->
	   let l = [getVar n (getNorth d i j);
		    getVar e (getEast d i j);
		    getVar s (getSouth d i j);
		    getVar o (getWest d i j);
		    getVar c (getId d i j)] in
	   (rules t i j d ((fold_and l)::f))
      in list := (fold_or (rules a i j d []))::!list;
    done;
  done;
  (fold_and !list);
;;

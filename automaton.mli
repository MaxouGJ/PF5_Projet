(*Les états possibles d'une cellule*)
type state = Alive | Dead
(*Une règle de l'automate*) (*Nord, Est, Sud, Ouest, Case*)
type rule = state * state * state * state * state
(*Une génération*)
type generation = state array array
(*Un automate*)
type automaton = rule list

(*Exception lancée lorsque le fichier n'est pas au format standard*)
exception Format_non_standard

(*Lit le fichier f au format standard, initialise un automate avec la génération spécifiée*)
val parse :  string -> int * (state * state * state * state * state) list * state array array

(*Affiche une grille g de taille n*)
val show_generation : state array array -> unit

(*Convertit une case et son entourage en rule*)
val next_generation :  (state * state * state * state * state) list -> generation -> state array array

(*Représente toutes les formules logiques*)
type formula =
    True
  | False
  | Var of int
  | And of formula * formula
  | Or of formula * formula
  | Neg of formula

(*Règles de bases*)
val baseRules : Automaton.rule list ref

(*Retire les règles stables de l'automate des règles de base*)
val triBaseRules : Automaton.automaton -> Automaton.rule list

(*Converti une règle en formule*)
val stables : Automaton.automaton -> int -> formula * int * int

(*Converti une formule en fichier dimacs*)
val create_dimacs : formula * int * int -> unit

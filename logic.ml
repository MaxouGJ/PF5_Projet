open Automaton;;

(*Représente toutes les formules logiques*)
type formula = True
		|False
		|Var of string
		|And of formula * formula
		|Or of formula * formula
		|Neg of formula;;


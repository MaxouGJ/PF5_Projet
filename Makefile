#Edition des liens de création de l'executable
projet : automaton.cmo logic.cmo main.cmo
	ocamlc -o projet automaton.cmo logic.cmo stable.cmo main.cmo

#Compilation du corps du module automaton
automaton.cmo : automaton.ml automaton.cmi
	ocamlc -c automaton.ml

#Compilation du corps du module automaton
automaton.cmi : automaton.mli
	ocamlc automaton.mli

#Compilation du corps du module logic
logic.cmo : logic.ml logic.cmi automaton.cmi
	ocamlc -c logic.ml

#Compilation du corps du module logic
logic.cmi : logic.mli
	ocamlc logic.mli

#Compilation du corps du module logic
stable.cmo : stable.ml automaton.cmi logic.cmi
	ocamlc -c stable.ml

#Compilation de l'interface du module main
main.cmo : main.ml automaton.cmi logic.cmi stable.cmo
	ocamlc -c main.ml

#Effacer les fichiers auxiliares
clean :
	rm *.cmi *.cmo

clear :
	rm entree.dimacs sortie

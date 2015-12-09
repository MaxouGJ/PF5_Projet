#Edition des liens de création de l'executable
projet : automaton.cmo logic.cmo main.cmo
	ocamlc -o projet automaton.cmo logic.cmo main.cmo

#Compilation du corps du module automaton
automaton.cmo : automaton.ml automaton.cmi
	ocamlc -c automaton.ml

#Compilation du corps du module automaton
automaton.cmi : automaton.mli
	ocamlc automaton.mli

#Compilation du corps du module logic
logic.cmo : logic.ml automaton.cmi
	ocamlc -c logic.ml

#Compilation de l'interface du module main
main.cmo : main.ml automaton.cmi logic.cmo
	ocamlc -c main.ml

#Effacer les fichiers auxiliares
clean :
	rm *.cmi *.cmo

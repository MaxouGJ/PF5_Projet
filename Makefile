#Edition des liens de création de l'executable
projet : automaton.cmo main.cmo
	ocamlc -o projet automaton.cmo main.cmo

#Compilation du corps du module automaton
automaton.cmo : automaton.ml automaton.cmi
	ocamlc -c automaton.ml

#Compilation du corps du module automaton
automaton.cmi : automaton.mli
	ocamlc automaton.mli

#Compilation de l'interface du module main
main.cmo : main.ml automaton.cmi
	ocamlc -c main.ml

#Effacer les  fichiers auxiliares
clean :
	rm *.cmi *.cmo

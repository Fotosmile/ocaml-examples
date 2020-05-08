all: gauss_elimination gauss_elimination_cmd
.PHONY: all clean

gauss_elimination: vec.ml mat.ml syst.ml main.ml
	ocamlc -o gauss_elimination vec.ml mat.ml syst.ml main.ml

gauss_elimination_cmd: vec.ml mat.ml syst.ml main_cmd.ml
	ocamlc -o gauss_elimination_cmd str.cma vec.ml mat.ml syst.ml main_cmd.ml

mat.ml: vec.ml
syst.ml: vec.ml mat.ml
main.ml: syst.ml
main_cmd.ml: syst.ml

clean:
	rm -rf gauss_elimination* *.cmi *.cmo
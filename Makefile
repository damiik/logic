.PHONY: all #don't refer to real files
all: ini.bytes #ini

#executable file
ini: ./table.ml ./logicUnit.ml ./logic.ml
	ocamlfind ocamlopt -I ./ -o logic ./table.ml ./logicUnit.ml ./logic.ml

#ocamlrun script executable (binary data)
ini.bytes: ./table.ml ./logicUnit.ml ./logic.ml
	ocamlfind ocamlc -I ./ -o -c logic.bytes ./table.ml ./logicUnit.ml ./logic.ml



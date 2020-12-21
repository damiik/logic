.PHONY: all #don't refer to real files
all: ini.bytes #ini

#executable file
ini: ./logicBase.ml ./logicUnit.ml ./logic.ml
	ocamlfind ocamlopt -I ./ -o logic ./logicBase.ml ./logicUnit.ml ./logic.ml

#ocamlrun script executable (binary data)
ini.bytes: ./logicBase.ml ./logicUnit.ml ./logic.ml
	ocamlfind ocamlc -I ./ -o ./logic.bytes ./logicBase.ml ./logicUnit.ml ./logic.ml



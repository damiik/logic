.PHONY: all #don't refer to real files
all: ini.bytes #ini

#executable file
ini: ./table.ml ./logicBase.ml ./logicUnit.ml ./logic.ml
	ocamlfind ocamlopt -I ./ -o logic ./table.ml ./logicBase.ml ./logicUnit.ml ./logic.ml

#ocamlrun script executable (binary data)
ini.bytes: ./table.ml ./logicBase.ml ./logicUnit.ml ./logic.ml
	ocamlfind ocamlc -I ./ -o ./logic.bytes ./table.ml ./logicBase.ml ./logicUnit.ml ./logic.ml



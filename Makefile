all: build

build:
	ocamlopt str.cmxa -o forth src/forth.ml

clean:
	rm src/*o src/*cmi src/*cmx forth

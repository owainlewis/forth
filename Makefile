all: build

build:
	ocamlopt str.cmxa -o forth src/mstack.ml src/forth.ml src/util.ml

clean:
	rm *o *cmi *cmx forth

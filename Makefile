all: build

build:
	ocamlopt str.cmxa -o forth forth.ml util.ml

clean:
	rm *o *cmi *cmx forth

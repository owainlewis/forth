all: build

build:
	ocamlopt str.cmxa -o forth mstack.ml forth.ml util.ml

clean:
	rm *o *cmi *cmx forth

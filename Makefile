.PHONY: test

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean

doc:
	dune build @doc

zip:
	rm -f MS2.zip
	zip -r MS2.zip . -x@exclude.lst
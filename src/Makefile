binary:
	dune build src/XpatSolver.exe

byte:
	dune build src/XpatSolver.bc

clean:
	dune clean

test:
	dune runtest
etat:
	ocamlc -c Card.mli
	ocamlc -c Card.ml
	ocamlc -c PArray.mli
	ocamlc -c PArray.ml
	ocamlc -c XpatRandomExemple.ml
	ocamlc -c Etat.ml
	ocamlc Card.cmo PArray.cmo XpatRandomExemple.cmo Etat.cmo -o out.exe

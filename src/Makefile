binary:
	dune build src/XpatSolver.exe

byte:
	dune build src/XpatSolver.bc

clean:
	dune clean

test:
	dune runtest
mesTests:
	ocamlc -c Card.mli
	ocamlc -c Card.ml
	ocamlc -c PArray.mli
	ocamlc -c PArray.ml
	ocamlc -c XpatRandomExemple.ml
	ocamlc -c Regle.ml
	ocamlc -c Etat.ml
	ocamlc -c Test.ml
	ocamlc Card.cmo Regle.cmo PArray.cmo XpatRandomExemple.cmo Etat.cmo Test.cmo -o out.exe

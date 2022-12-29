binary:
	dune build src/XpatSolver.exe

byte:
	dune build src/XpatSolver.bc

clean:
	rm *.cmi
	rm *.cmo
	dune clean

test:
	dune runtest
mesTests:
	ocamlc -c Card.mli
	ocamlc -c Card.ml
	ocamlc -c PArray.mli
	ocamlc -c PArray.ml
	ocamlc -c Fifo.mli
	ocamlc -c Fifo.ml
	ocamlc -c XpatRandomExemple.ml
	ocamlc -c XpatRandom.mli
	ocamlc -c XpatRandom.ml
	ocamlc -c Regle.ml
	ocamlc -c Config.ml
	ocamlc -c Etat.ml
	ocamlc -c Searching.ml
	ocamlc -c Test.ml
	
	ocamlc Card.cmo Regle.cmo PArray.cmo XpatRandomExemple.cmo Fifo.cmo XpatRandom.cmo Config.cmo Etat.cmo Searching.cmo Test.cmo -o out.exe

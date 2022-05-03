nom = puissance4

all:
	ocamlmktop -o $(nom) graphics.cma  $(nom).ml;
clean:
	rm -rf $(nom) *.cmi *.cmo *~

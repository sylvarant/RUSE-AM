all: miniML 

GENFILES=miniMLparser.ml miniMLparser.mli miniMLlexer.ml \
  miniCparser.ml miniCparser.mli miniClexer.ml

MINIML=modules.cmo miniML.cmo adriaan.cmo miniMLparser.cmo miniMLlexer.cmo miniMLmain.cmo

miniML: $(MINIML)
	ocamlc -o miniML $(MINIML)

miniMLparser.ml miniMLparser.mli: miniMLparser.mly
	ocamlyacc miniMLparser.mly

miniMLlexer.ml: miniMLlexer.mll
	ocamllex miniMLlexer.mll

clean:
	rm -f *.cm?
	rm -f miniML
	rm -f miniC
	rm -f $(GENFILES)

.SUFFIXES: .cmo .ml .cmi .mli

.ml.cmo:
	ocamlc -c $<
.mli.cmi:
	ocamlc -c $<

include .depend

depend: $(GENFILES)
	ocamldep *.ml *.mli > .depend


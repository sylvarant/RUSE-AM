TARGET=miniml

SOURCES = \
	syntax.ml \
	type_check.ml \
	eval.ml \
	machine.ml \
	compile.ml \
	lexer.mll \
	parser.mly \
	miniml.ml

OCAMLBUILD=ocamlbuild

default: native

all: byte native html doc

byte:
	$(OCAMLBUILD) $(TARGET).byte

native:
	$(OCAMLBUILD) $(TARGET).native

clean:
	$(OCAMLBUILD) -clean

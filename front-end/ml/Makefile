#########################################
#     Custom MakeFile                   #
# By Adriaan Larmuseau                  #
#########################################


#============================================
# Tools
#============================================

OCAMLBUILD = ocamlbuild
FINDFLAGS = -package bisect -syntax camlp4o

#============================================
# Executable Targets
#============================================

TARGET = main


#============================================
# Compilation sources
#============================================

SOURCES = $(wildcard *.ml*)


#============================================
# Linking
#============================================

native:
	$(OCAMLBUILD) -use-ocamlfind $(FINDFLAGS)  $(TARGET).native


#============================================
# Results
#============================================
all: native


clean:
	$(OCAMLBUILD) -clean

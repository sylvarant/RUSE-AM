#########################################
#     Custom MakeFile                   #
# By Adriaan Larmuseau                  #
#########################################

#============================================
# Tools
#============================================

LD = gcc
SLD = sancus-ld

CC = gcc
SCC = sancus-cc

SHELL := /bin/bash

#============================================
# Platform specific settings
#============================================
all:

uname_S = $(shell sh -c 'uname -s 2>/dev/null || echo not')

ifeq ($(uname_S),Darwin)
 	DEF_TARGET = darwin	
	NAME_LIB = -install_name
	LD = clang
	CC = clang
endif

ifeq ($(uname_S),Linux)
	DEF_TARGET = linux
	NAME_LIB = -soname
endif



#============================================
# Executable Targets
#============================================

EXEC= ../bin/ruse$(subst -D,.,$(LANG))$(subst -D,.,$(BYTEOPT))$(subst -D,.,$(DEBOPT))
SANEXEC = ../bin/exec.elf
SANEXEC_NO_HMAC = ../bin/exec-no-hmac.elf

#============================================
# Linkable Targets
#============================================

LIBSO = ../lib/libruse.so
LIBA  = $(LIBSO).1
LIBB  = $(LIBSO).1.0.1 

#============================================
# Flags
#============================================

CFLAGS = $(BYTEOPT) $(DEBOPT) $(CWARNFLAGS) -g -fprofile-arcs -ftest-coverage
SANFLAGS = -DSANCUS_SPM $(FPGA)
MACFLAGS= -std=gnu99
SANLDFLAGS = --rom-size 48K --ram-size 10K  --standalone --sm-stack-size=2048
KEY = deadbeefcafebabedeadbeefcafebabe

#============================================
# Compilation sources
#============================================

# compile all c files in buildsrc
SOURCE=$(wildcard *.c)
LIBSOURCE= $(filter-out main.c,$(SOURCE))

# generate a set of insecure objects with main.o
INSECURE= $(SOURCE:.c=.o)
SANINSECURE= $(SOURCE:.c=.san_o)

# generate the objects for the library
LIBINSECURE=$(LIBSOURCE:.c=.o)
LIBSANSECURE=$(LIBSOURCE:.c=.san_o)

# generate a set of secure objects
SECURE= $(LIBSOURCE:.c=.o_S)
SANSECURE= $(LIBSOURCE:.c=.san_o_S)

#============================================
# Compiling Objects
#============================================

%.o: %.c
	$(CC) $(CFLAGS) $(MACFLAGS) -c -o $@ $<	

%.san_o: %.c 
	$(SCC) $(CFLAGS) $(SANFLAGS) -c -o $@ $<	

%.o_S: %.c
	$(CC) $(CFLAGS) $(MACFLAGS) -DSECURE -c -o $@ $<	

%.san_o_S: %.c
	$(SCC) $(CFLAGS) $(SANFLAGS) -DSECURE -c -o $@ $<	

#============================================
# Linking
#============================================

#todo parameterize inlinking
darwin: $(INSECURE) $(SECURE)
	$(LD)  --coverage -o $(EXEC) $^

linux: $(INSECURE) $(SECURE)
	$(LD) -lgcov  -o $(EXEC) $^

$(SANEXEC_NO_HMAC): $(SANINSECURE) $(SANSECURE)
	$(SLD) $(SANLDFLAGS) -o  $@ $^ libdev.a

sancus: $(SANEXEC_NO_HMAC) 
	sancus-hmac --verbose --key $(KEY) -o $(SANEXEC) $(SANEXEC_NO_HMAC)

library: $(CFLAGS) += -fPIC
library: $(LIBINSECURE) $(SECURE)
	$(CC) -shared -Wl,$(NAME_LIB),$(LIBA) -o $(LIBB) $^
	ln -sf  $(LIBB) $(LIBSO)
	ln -sf  $(LIBB) $(LIBA)

#============================================
# Results
#============================================

all: $(DEF_TARGET) 

#============================================
# Helpers
#============================================

clean:
	$(RM)  *.c *.h *.gcno $(SECURE) $(INSECURE) $(SANSECURE) $(SANINSECURE)



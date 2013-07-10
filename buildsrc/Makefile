#########################################
#     Custom MakeFile                   #
# By Adriaan Larmuseau                  #
#########################################

#============================================
# Platform specific settings
#============================================

ifeq ($(uname_S),Darwin)
	DEF_TARGET = darwin	
endif

ifeq ($(uname_S),Linux)
	DEF_TARGET = linux
endif


#============================================
# Tools
#============================================

LD = gcc
SLD = sancus-ld

CC = gcc
SCC = sancus-cc

SHELL := /bin/bash

#============================================
# Executable Targets
#============================================

EXEC= ../bin/ruse.$(LANG).$(BYTEOPT)
SANEXEC = ../bin/exec.elf
SANEXEC_NO_HMAC = ../bin/exec-no-hmac.elf

#============================================
# Flags
#============================================

CFLAGS = $(BYTEOPT) $(DEBOPT) $(CWARNFLAGS) -g
SANFLAGS = --verbose -DSPM
MACFLAGS= -std=gnu99
SANLDFLAGS = --rom-size 48K --ram-size 10K --verbose --standalone
KEY = deadbeefcafebabedeadbeefcafebabe

#============================================
# Compilation sources
#============================================

# compile all c files in buildsrc
SOURCE=binding.c cesk.c continuation.c garbage_collection.c label.c load.c main.c scheme.c string.c 

# generate a set of insecure objects
INSECURE= $(SOURCE:.c=.o)
SANINSECURE= $(SOURCE:.c=.san_o)

# generate a set of secure objects
SECURE= $(INSECURE:.o=.o_S)
SANSECURE= $(SOURCE:.c=.san_o_S)


#============================================
# Compiling objects
#============================================

%.o_I: %.bin
	ld -r -b binary -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) $(MACFLAGS) -c -o $@ $<	

%.san_o: %.c 
	$(SCC) $(CFLAGS) $(SANFLAGS) -c -o $@ $<	

%.o_S: $(SOURCE)
	$(CC) $(CFLAGS) $(MACFLAGS) -DSECURE -c -o $@ $<	

%.san_o_S: $(SOURCE)
	$(SCC) $(CFLAGS) $(SANFLAGS) -DSECURE -c -o $@ $<	


#============================================
# Linking
#============================================


#todo parameterize inlinking
darwin: $(INSECURE) $(SECURE)
	$(LD) -o $(EXEC) $^

$(SANEXEC_NO_HMAC): $(SANINSECURE) $(SANSECURE) $(INPUT)
	$(SLD) $(SANLDFLAGS) -o  $@ $^

sancus: $(SANEXEC_NO_HMAC) 
	sancus-hmac --verbose --key $(KEY) -o $(SANEXEC) $(SANEXEC_NO_HMAC)

#============================================
# Results
#============================================

all: $(DEF_TARGET) 


#============================================
# Helpers
#============================================

clean:
	$(RM) *.o *.o_S *.c *.h *.parser


#########################################
#     Custom MakeFile                   #
# By Adriaan Larmuseau                  #
#########################################


#============================================
# Tools
#============================================

MLD = gcc
SLD = sancus-ld

MCC = gcc
SCC = sancus-cc

SHELL := /bin/bash

#============================================
# Executable Targets
#============================================

MACEXEC= ../bin/exec
SANEXEC = ../bin/exec.elf
SANEXEC_NO_HMAC = ../bin/exec-no-hmac.elf

#============================================
# Flags
#============================================

CFLAGS = $(DEBOPT) $(CWARNFLAGS) -g
SANFLAGS = --verbose -DSPM
MACFLAGS= -std=gnu99
SANLDFLAGS = --rom-size 48K --ram-size 10K --verbose --standalone
KEY = deadbeefcafebabedeadbeefcafebabe

#============================================
# Compilation sources
#============================================

# compile all c files in buildsrc
SOURCE= $(shell find *.c) 

# generate a set of insecure objects
INSECURE= $(SOURCE:.c=.o)
SANINSECURE= $(SOURCE:.c=.san_o)

# generate a set of secure objects
SECURE= $(INSECURE:.o=.o_S)
SANSECURE= $(SOURCE:.c=.san_o_S)


#============================================
# Compiling objects
#============================================

%.o: %.c
	$(MCC) $(CFLAGS) $(MACFLAGS) -c -o $@ $<	

%.san_o: %.c
	$(SCC) $(CFLAGS) $(SANFLAGS) -c -o $@ $<	

%.o_S: %.c
	$(MCC) $(CFLAGS) $(MACFLAGS) -DSECURE -c -o $@ $<	

%.san_o_S: %.c
	$(SCC) $(CFLAGS) $(SANFLAGS) -DSECURE -c -o $@ $<	


#============================================
# Linking
#============================================

mac: $(INSECURE) $(SECURE)
	$(MLD)  -o $(MACEXEC) $(SECURE) $(INSECURE) 

$(SANEXEC_NO_HMAC): $(SANINSECURE) $(SANSECURE)
	$(SLD) $(SANLDFLAGS) -o  $@ $^

sancus: $(SANEXEC_NO_HMAC) 
	sancus-hmac --verbose --key $(KEY) -o $(SANEXEC) $(SANEXEC_NO_HMAC)

#============================================
# Results
#============================================

all: mac sancus


#============================================
# Helpers
#============================================

clean:
	$(RM) *.o* $(NAME) *~ *.c *.h *.clean


#########################################
#     Custom MakeFile                   #
# By Adriaan Larmuseau                  #
#########################################


#============================================
# Tools
#============================================

MLD = gcc

MCC = gcc

SHELL := /bin/bash

#============================================
# Executable Targets
#============================================

MACEXEC= ../bin/exec


#============================================
# Flags
#============================================

MACFLAGS= -std=gnu99
CFLAGS = $(DEBOPT) $(CWARNFLAGS) -g


#============================================
# Compilation sources
#============================================

# compile all c files in buildsrc
SOURCE= $(shell find *.c) 

# generate a set of insecure objects
INSECURE= $(SOURCE:.c=.o)

# generate a set of secure objects
SECURE= $(INSECURE:.o=.o_S)


#============================================
# Compiling objects
#============================================

%.o: %.c
	$(MCC) $(CFLAGS) $(MACFLAGS) -c -o $@ $<	

%.o_S: %.c
	$(MCC) $(CFLAGS) $(MACFLAGS) -DSECURE -c -o $@ $<	


#============================================
# Linking
#============================================

maclink: $(INSECURE) $(SECURE)
	$(MLD)  -o $(MACEXEC) $(SECURE) $(INSECURE) -lm # TODO remove math dependency !


#============================================
# Results
#============================================

all: maclink


#============================================
# Helpers
#============================================

clean:
	$(RM) *.o* $(NAME) *~ *.c *.h *.clean


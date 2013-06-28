#########################################
#     Custom MakeFile For MAC testing   #
# By Adriaan Larmuseau                  #
#########################################

# no bash no play
SHELL := /bin/bash

# program name
NAME= ../bin/exec

# to be linked objects auto add all c objects with oneliner
CSOURCES= $(shell find *.c | sed -e "s/\.c/.o/")


CFLAGS= -g $(DEBOPT) $(CWARNFLAGS) -std=gnu99 -DSECURE # TODO parameterize

all: $(CSOURCES) link

clean:
	$(RM) *.o $(NAME) *~ *.c *.h

link:
	gcc  -o $(NAME) $(CSOURCES) -lm

tarball:
	tar -cf $(NAME).tar *.c *.h *.scm run Makefile 

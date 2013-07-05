/*
 * =====================================================================================
 *
 *       Filename:  global.h
 *
 *    Description: global header file
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#ifndef GLOBAL_INCLUDED
#define GLOBAL_INCLUDED

#include <stdlib.h> // TODO remove duplicates

#ifdef SECURE
#include "secure_macro.h"
#else
#include "insecure_macro.h"
#endif

/*-----------------------------------------------------------------------------
 *  DEBUGGING
 *-----------------------------------------------------------------------------*/

#ifdef DEBUG
    #include <stdio.h>
    #define DEBUG_PRINT(...) \
	do{	fprintf(stderr,"DEBUG:: "); fprintf(stderr,__VA_ARGS__); fprintf(stderr,"\n");fflush(stderr);} while(0)
#else
    #define DEBUG_PRINT(...) do {} while (0)
#endif


/*-----------------------------------------------------------------------------
 *  Secure Entrypoints 
 *-----------------------------------------------------------------------------*/
ENTRYPOINT void * secure_eval(int);
ENTRYPOINT void sload (void);


/*-----------------------------------------------------------------------------
 *  InSecure Hooks 
 *-----------------------------------------------------------------------------*/
HOOK void * evaluate(void *); 
HOOK void ** getinput(void); 
HOOK int getinput_n(void);

#endif

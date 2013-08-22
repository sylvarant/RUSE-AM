/*
 * =====================================================================================
 *
 *       Filename:  global.h
 *
 *    Description:  global header file
 *                  These definitions are not to be duplicated accross the
 *                  boundary
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */


#ifndef GLOBAL_INCLUDED
#define GLOBAL_INCLUDED

#include "spm.h"

/*-----------------------------------------------------------------------------
 *  DEBUGGING
 *-----------------------------------------------------------------------------*/

#ifdef DEBUG
    #include <stdlib.h> // TODO remove duplicates
    #include <stdio.h>
    #define DEBUG_PRINT(...) \
	do{	fprintf(stderr,"DEBUG:: "); fprintf(stderr,__VA_ARGS__); fprintf(stderr,"\n");fflush(stderr);} while(0)
#else
    #define DEBUG_PRINT(...) do {} while (0)
#endif


/*-----------------------------------------------------------------------------
 *  LANGUAGES
 *-----------------------------------------------------------------------------*/
enum LANGUAGE {SCHEME,ML};


/*-----------------------------------------------------------------------------
 *  Secure Entrypoints 
 *-----------------------------------------------------------------------------*/
ENTRYPOINT void * secure_eval(int);
ENTRYPOINT void sload(char *,void * callback(void *));


/*-----------------------------------------------------------------------------
 *  InSecure Hooks 
 *-----------------------------------------------------------------------------*/
HOOK void * evaluate(void *); 

#endif

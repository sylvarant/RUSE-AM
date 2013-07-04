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
    #define DEBUG_PRINT(x) printf("DEBUG:: "); printf x ; printf("\n");fflush(stdout);
#else
    #define DEBUG_PRINT(x) do {} while (0);
#endif


/*-----------------------------------------------------------------------------
 *  Secure Entrypoints - TODO macrofy
 *-----------------------------------------------------------------------------*/
ENTRYPOINT void * secure_eval(int);
ENTRYPOINT void sload (void);


//TODO add insecure Entrypoints

#endif

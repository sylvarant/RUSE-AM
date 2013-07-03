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


#include "secure_macro.h"

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
ENTRYPOINT void * secure_eval(int seccode);
ENTRYPOINT void sload (void);


//TODO at insecure Entrypoints

#endif

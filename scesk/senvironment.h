/*
 * =====================================================================================
 *
 *       Filename:  senvironment.h
 *
 *    Description:  The environment structure
 *
 *        Created:  06/28/2013 14:46:42
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#ifndef ENVIRONMENT_INCLUDED
#define ENVIRONMENT_INCLUDED

#include "global.h"

/*-----------------------------------------------------------------------------
 *  Data Structure
 *-----------------------------------------------------------------------------*/

struct N(envnode) {
    char *key;
    int value;
    struct N(envnode) * next;

};

typedef struct N(environ_t) {
    struct N(envnode) * bucket;
    int size;
}N(environ);


/*-----------------------------------------------------------------------------
 *  Functionality
 *-----------------------------------------------------------------------------*/
FUNCTIONALITY int N(insert)(N(environ) *table,char *key,int value);
FUNCTIONALITY int N(get)(N(environ) *table,const char *key);
FUNCTIONALITY N(environ) * N(copyenv)( N(environ) * table);

#endif

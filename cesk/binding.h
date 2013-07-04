/*
 * =====================================================================================
 *
 *       Filename:  binding.h
 *
 *    Description:  Implementation of the environment
 *
 *        Created:  06/28/2013 14:46:42
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#define ENVIRONMENT_INCLUDED

#include "global.h"

/*-----------------------------------------------------------------------------
 *  Environment Binding
 *  Binds a key -> value
 *-----------------------------------------------------------------------------*/
typedef struct N(Binding_t) {
    char *key;
    int value;
    struct N(Binding_t) * next;

}BINDING;


/*-----------------------------------------------------------------------------
 *  Functionality
 *-----------------------------------------------------------------------------*/
FUNCTIONALITY void N(insertBinding)(BINDING **,char *,int);
FUNCTIONALITY int N(getBinding)(BINDING *,const char *);
FUNCTIONALITY BINDING * N(copyBinding)(BINDING *);


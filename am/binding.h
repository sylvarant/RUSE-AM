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


#ifndef ENVIRONMENT_INCLUDED
#define ENVIRONMENT_INCLUDED

#include "global.h"

// will be overwritten by the use of ruse_all
#include "insecure_macro.h"

/*-----------------------------------------------------------------------------
 *  Environment Binding
 *  Binds a key -> value
 *-----------------------------------------------------------------------------*/
typedef struct N(Binding_t) {
    char *key;
    union N(Value_u) * address;
    struct N(Binding_t) * next;

}BINDING;


/*-----------------------------------------------------------------------------
 *  Functionality
 *-----------------------------------------------------------------------------*/
FUNCTIONALITY union N(Value_u) * N(insertBinding)(BINDING **,char *);
FUNCTIONALITY union N(Value_u) * N(getBinding)(BINDING *,char *);

// Deprecated
// FUNCTIONALITY BINDING * N(copyBinding)(BINDING *);

#endif

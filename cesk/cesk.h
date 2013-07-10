/*
 * =====================================================================================
 *
 *       Filename:  cesk.h
 *
 *    Description:  Defines the CESK structures 
 *
 *        Created:  07/02/2013 15:52:31
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#ifndef CESK_INCLUDED
#define CESK_INCLUDED

#ifdef SECURE

// load insecure header
#include "undefine.h"
#include "insecure_macro.h"
#include "scheme.h"

// load secure header
#include "undefine.h"
#define SECURE
#include "secure_macro.h"
#include "scheme.h"

#else

// insecure does not need fancy tricks
#include "scheme.h"

#endif

#include "label.h"

/*-----------------------------------------------------------------------------
 *  Types
 *-----------------------------------------------------------------------------*/
typedef VALUE * Memory;


/*-----------------------------------------------------------------------------
 *  CESK state
 *-----------------------------------------------------------------------------*/
typedef struct N(State_t){
    
    // the control statement
    VALUE control;

    // the environment the closes the control
    BINDING * env; 

    // the storage mapping locations to VALUE
    Memory storage;

    // the current continuation
    KONT cont;

    // when secure add labelling
    #ifdef SECURE
    Label * label;
    #endif

    // the current free address TODO -> garbage collection remove this
	int free_adr;
}STATE;


/*-----------------------------------------------------------------------------
 *  Processing Tuple
 *-----------------------------------------------------------------------------*/
typedef union N(Limbo_u){
    
    // when computation is unfinished this union is empty
    void * empty; 

    // an answer was produced
    VALUE answer;
}LIMBO;

/*-----------------------------------------------------------------------------
 *  Functionality
 *-----------------------------------------------------------------------------*/
FUNCTIONALITY VALUE N(steprec)(void);
FUNCTIONALITY void N(inject)(void);


/*-----------------------------------------------------------------------------
 *  Cesk specific hooks
 *-----------------------------------------------------------------------------*/
HOOK void run(VALUE *,int c);

#endif

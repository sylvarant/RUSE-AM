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

// secure and insecure definitions
#include "ruse_all.h"
#include "label.h"

#else

// insecure does not need fancy tricks
#include "ruse.h"

#endif


/*-----------------------------------------------------------------------------
 *  CESK state
 *  => the storage is the heap !!
 *-----------------------------------------------------------------------------*/
typedef struct N(State_t){
    
    // the control statement
    VALUE control;

    // the environment the closes the control
    BINDING * env; 

    // the current continuation
    KONT cont;

    // when secure add labelling
    #ifdef SECURE
    LABEL * label;
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
 *  Global Variables
 *-----------------------------------------------------------------------------*/
FUNCTIONALITY enum LANGUAGE N(language);

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

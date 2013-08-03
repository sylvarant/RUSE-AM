/*
 * =====================================================================================
 *
 *       Filename:  type.c
 *
 *    Description:  Ruse type descriptors
 *
 *        Created:  07/22/2013 13:55:25
 *
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */


#include "cesk.h"


/*-----------------------------------------------------------------------------
 *  Local functions
 *-----------------------------------------------------------------------------*/
#ifdef Secure
LOCAL int checkML(OTHERVALUE term,TYPE goal);
#endif


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeTIgnore
 *  Description:    make the Ignore type -> this is the type of scheme
 * =====================================================================================
 */
FUNCTIONALITY void * N(makeTIgnore)(){
    return (void*) N(TIGNORE);
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeTUnit
 *  Description:    make the Unit type 
 * =====================================================================================
 */
FUNCTIONALITY void * N(makeTUnit)(){
    return (void*) N(TUNIT);
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeTInt
 *  Description:    make the Int type 
 * =====================================================================================
 */
FUNCTIONALITY void * N(makeTInt)(){
    return (void *) N(TINT);
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeTBoolean
 *  Description:    make the Boolean type 
 * =====================================================================================
 */
FUNCTIONALITY void * N(makeTBoolean)(){
    return (void *) N(TBOOLEAN);
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeTArrow
 *  Description:    make the Arrow type 
 * =====================================================================================
 */
FUNCTIONALITY void * N(makeTArrow)(void * l,void * r){
    TYPE t;
    t.a    = MALLOC(sizeof(struct N(TArrow)));
    t.a->t = N(TARROW);
    t.a->left.byte  = l;
    t.a->right.byte = r;
    return t.a; 
}

#ifdef SECURE

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    N(checkML)
 *  Description:    typechecks insecure Values
 * =====================================================================================
 */
LOCAL int checkML(OTHERVALUE term,TYPE goal){
   
    switch(term.b->t){

        case OTHERN(BOOLEAN) : return (goal.byte == N(makeTBoolean)());

        case OTHERN(INT) : return (goal.byte == N(makeTInt)());

        case OTHERN(UNIT) : return (goal.byte == N(makeTUnit)());

        default : {
            DEBUG_PRINT("Term cannot be secure ML type checked");    
            exit(1);
        }
    }

    return 0;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    N(checkType)
 *  Description:    typechecks insecure Values
 * =====================================================================================
 */
FUNCTIONALITY int N(checkType)(void * l,TYPE goal){
    OTHERVALUE t;
    t.b = l;

    switch(N(language)){

        case SCHEME : return 1;

        case ML : return checkML(t,goal);

        default : {
            DEBUG_PRINT("Language not supported !");
            exit(1);
        }
    }
}


#endif

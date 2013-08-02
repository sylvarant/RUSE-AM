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
LOCAL int checkML(OTHERVALUE term,OTHERTYPE goal);
#endif


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeTIgnore
 *  Description:    make the Ignore type -> this is the type of scheme
 * =====================================================================================
 */
FUNCTIONALITY void * N(makeTIgnore)(){
    static enum  N(TTag) t = N(TIGNORE);
    return &t;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeTUnit
 *  Description:    make the Unit type 
 * =====================================================================================
 */
FUNCTIONALITY void * N(makeTUnit)(){
    static struct N(TUnit) unit_type = {N(TUNIT)};
    return &unit_type; 
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeTInt
 *  Description:    make the Int type 
 * =====================================================================================
 */
FUNCTIONALITY void * N(makeTInt)(){
    static struct N(TInt) int_type  = {N(TINT)};
    return &int_type; 
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeTBoolean
 *  Description:    make the Boolean type 
 * =====================================================================================
 */
FUNCTIONALITY void * N(makeTBoolean)(){
    static struct N(TBoolean) bool_type = {N(TBOOLEAN)};
    return &bool_type;
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
    t.a->left.b  = l;
    t.a->right.b = r;
    return t.a; 
}

#ifdef SECURE

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    N(checkML)
 *  Description:    typechecks insecure Values
 * =====================================================================================
 */
LOCAL int checkML(OTHERVALUE term,OTHERTYPE goal){
   
    switch(term.b->t){

        case OTHERN(BOOLEAN) : return (goal.b == OTHERN(makeTBoolean)());

        case OTHERN(INT) : return (goal.i == OTHERN(makeTInt)());

        case OTHERN(UNIT) : return (goal.u == OTHERN(makeTUnit)());

        default : {
            DEBUG_PRINT("Term cannot be ML type checked");    
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
FUNCTIONALITY int N(checkType)(void * l,OTHERTYPE goal){
    OTHERVALUE t;
    t.b = l;

    switch(goal.b->t){

        case N(TIGNORE) : 
            // secure language is scheme : do nothing
            return 1;

        default :
            return checkML(t,goal);

    }
}


#endif

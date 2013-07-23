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


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeTIgnore
 *  Description:    make the Ignore type -> this is the type of scheme
 * =====================================================================================
 */
FUNCTIONALITY void * N(makeTIgnore)(){
    TYPE t;
    t.tt = N(TIGNORE);
    return t.i;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeTUnit
 *  Description:    make the Unit type 
 * =====================================================================================
 */
FUNCTIONALITY void * N(makeTUnit)(){
    TYPE t;
    t.u    = MALLOC(sizeof(struct N(TUnit)));
    t.u->t = N(TUNIT);
    return t.b; 
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeTInt
 *  Description:    make the Int type 
 * =====================================================================================
 */
FUNCTIONALITY void * N(makeTInt)(){
    TYPE t;
    t.i    = MALLOC(sizeof(struct N(TInt)));
    t.i->t = N(TINT);
    return t.b; 
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeTBoolean
 *  Description:    make the Boolean type 
 * =====================================================================================
 */
FUNCTIONALITY void * N(makeTBoolean)(){
    TYPE t;
    t.b    = MALLOC(sizeof(struct N(TBoolean)));
    t.b->t = N(TBOOLEAN);
    return t.b; 
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
    t.a->left->b  = l;
    t.a->right->b = r;
    return t.a; 
}


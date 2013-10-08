/*
 * =====================================================================================
 *
 *       Filename:  continuation.c
 *
 *    Description:  The continuations
 *
 *        Created:  07/03/2013 16:10:41
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#include "cesk.h" 

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeKLet
 *  Description:    make the Let continuation
 * =====================================================================================
 */
FUNCTIONALITY void * N(makeKLet)(void * var,void * expr,BINDING* env,void * cont){
    KONT kk,arg;
    VALUE a, b;
    a.b        = var;
    b.b        = expr;
    arg.empty  =cont;
    kk.l       = MALLOC(sizeof(struct N(KLet)));
    kk.l->var  = a;
    kk.l->expr = b;
    kk.l->e    = env; 
    kk.l->next = arg;
    kk.l->t    = N(KLET);
    return kk.empty;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeKCont
 *  Description:    make the Continue continuation
 * =====================================================================================
 */
FUNCTIONALITY void * N(makeKCont)(BINDING* env,void * cont){
    KONT kk,arg; 
    arg.empty  = cont;
    kk.c       = MALLOC(sizeof(struct N(KCont)));
    kk.c->t    = N(KCONTINUE);
    kk.c->next = arg;
    kk.c->e    = env;
    return kk.empty;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeKRet
 *  Description:    make the Return continuation
 * =====================================================================================
 */
FUNCTIONALITY void * N(makeKRet)(void * cont){
    KONT kk,arg;
    arg.empty  = cont;
    kk.r       = MALLOC(sizeof(struct N(KRet)));
    kk.r->t    = N(KRET);
    kk.r->next = arg;
    return kk.empty;
}

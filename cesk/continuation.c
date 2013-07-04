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
FUNCTIONALITY KONT N(makeKLet)(union N(Value_u) var,union N(Value_u) expr,BINDING* env,KONT cont){
    KONT kk;
    kk.l       = MALLOC(sizeof(struct N(KLet)));
    kk.l->var  = var;
    kk.l->expr = expr;
    kk.l->e    = env; 
    kk.l->next = cont;
    kk.l->t    = N(KLET);
    return kk;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeKCont
 *  Description:    make the Continue continuation
 * =====================================================================================
 */
FUNCTIONALITY KONT N(makeKCont)(BINDING* env,KONT cont){
    KONT kk; 
    kk.c       = MALLOC(sizeof(struct N(KCont)));
    kk.c->t    = N(KCONTINUE);
    kk.c->next = cont;
    kk.c->e    = N(copyBinding)(env);
    return kk;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeKRet
 *  Description:    make the Return continuation
 * =====================================================================================
 */
FUNCTIONALITY KONT N(makeKRet)(KONT cont){
    KONT kk;
    kk.r       = MALLOC(sizeof(struct N(KRet)));
    kk.r->t    = N(KRET);
    kk.r->next = cont;
    return kk;
}

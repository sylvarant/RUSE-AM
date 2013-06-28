/*
 * =====================================================================================
 *
 *       Filename:  garbage_collection.c
 *
 *    Description:  Functions to clear memory
 *
 *        Created:  06/28/2013 11:12:14
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#include "garbage_collection.h"

#ifdef GARBAGE_COLLECTION_INCLUDED

#include <stdarg.h> // TODO remove
#include "senvironment.h" // TODO

/*-----------------------------------------------------------------------------
 *  Local functions
 *-----------------------------------------------------------------------------*/

LOCAL void clearvalue(int c,void * s,VALUE * p,...);
LOCAL void clearvaluels(int c,void * s,VALUE * ls);
LOCAL void emptyenv(N(environ) * env);


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    clearvalue
 *  Description:    clear a VALUE with c subvalues
 * =====================================================================================
 */
LOCAL void clearvalue(int c,void * s,VALUE * p,...){

    va_list arguments;
    va_start(arguments, p); 
    N(freevalue)(p);

    for(int i = 1; i < c; ++i ){
        N(freevalue)(va_arg(arguments,VALUE *));
    }

    free(s); 
    va_end(arguments);
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    sclearvalue*
 *  Description:    clear VALUE with a list of c subvalues 
 *                  TODO merge with other ?
 * =====================================================================================
 */
LOCAL void clearvaluels(int c,void * s,VALUE * ls){

    for(int i = 0 ; i < c ; i++){
       N(freevalue)(&ls[i]);
    }

    if(ls != NULL) free(ls);
    free(s);
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    emptyenv
 *  Description:    clear an environment -> this part of garbage collection as
 *                      -> The contents of an environment form direct or indirect
 *                      references to language descriptors
 * =====================================================================================
 */
LOCAL void emptyenv(N(environ) * table){

    struct N(envnode) *node;
    struct N(envnode) *nnode;

    node = table->bucket;
    nnode = node;

    while(node) {
        node = node->next;
        free(nnode); // TODO fix this massive leak
        nnode = node;
    }
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    freevalue
 *  Description:    free a value : delete both its substructures and itself
 *                  -> to be used by the garbage collector, do not freevalues
 *                  statically !
 * =====================================================================================
 */
FUNCTIONALITY void sfreevalue(VALUE * par){

    
    switch(par->tt){

        case N(VOID):
        case N(UNDEF): return;
    }

    switch(par->b->t){
    
        case N(INT) :  
            return free(par->z);
    
        #ifdef SECURE
        case SI : 
            return free(par->i);
        #endif

        // Booleans are optimized into a static true & false presence
        case N(BOOLEAN) : return; 

        case N(LAM) : {
            N(freevalue)(&par->l->body);
            return clearvaluels(par->l->nargs,par->l,par->l->arguments);
        }
    
        case N(PRIM) :
            return clearvaluels(par->p->nargs,par->p,par->p->arguments);

        case N(SYMBOL) :
            return free(par->s);

        case N(APPLICATION) :
            return clearvaluels(par->a->nargs,par->a,par->a->arguments);

        case N(IF) :
            return clearvalue(3,par->f,&par->f->cond,&par->f->cons,&par->f->alt);
    
        case N(CLOSURE) : {
            emptyenv(par->c->env);
            free(par->c->env);
            return clearvalue(1,par->c,&(par->c->lambda));
        }

        case N(CONTINUATION) :
            return free(par->k);
    
        case N(CALLCC) :
            return clearvalue(1,par->cc,&par->cc->function);     
    
        case N(SET) :
            return clearvalue(2,par->sv,&par->sv->var,&par->sv->value);
    
        case N(LET) :
            return clearvalue(3,par->lt,&par->lt->var,&par->lt->expr,&par->lt->body);
    
        case N(LETREC) :
        sfreevalue(&par->lr->body);
        for(int i = 0; i < par->lr->nargs; i++){
            N(freevalue)(&par->lr->vars[i]);
            N(freevalue)(&par->lr->exprs[i]);
        }
        free(par->lr->vars);
        free(par->lr->exprs);
        return free(par->lr);
    
    case N(BEGIN) :
        return clearvaluels(par->bg->nargs,par->bg,par->bg->stmts);
    
    case N(CAR) :
        return clearvalue(1,par->car,&par->car->arg); 
    
    case N(CDR) :
        return clearvalue(1,par->cdr,&par->cdr->arg); 
    
    case N(CONS) :
        return clearvalue(2,par->cons,&par->cons->arg,&par->cons->arg2); 
    
    case N(LIST) :
        return clearvaluels(par->ls->nargs,par->ls,par->ls->args);
    
    case N(QUOTE) :
        return clearvalue(1,par->q,&par->q->arg); 
    
    case N(PAIRQ) :
        return sclearvalue(1,par->pq,&par->pq->arg); 
    
    case N(LISTQ) :
        return sclearvalue(1,par->lq,&par->lq->arg); 

    case N(NULLQ) :
        return sclearvalue(1,par->nq,&par->nq->arg); 

    case N(DEFINE) :
        return sclearvalue(2,par->d,&par->d->var,&par->d->expr);

    default :
        DEBUG_PRINT(("Could not clear SValue !!!")) 
        return;
    
}}


#endif

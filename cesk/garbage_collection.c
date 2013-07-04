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
#include "binding.h" 

/*-----------------------------------------------------------------------------
 *  Local functions
 *-----------------------------------------------------------------------------*/

LOCAL void clearValue(int c,void * s,VALUE * p,...);
LOCAL void clearValuels(int c,void * s,VALUE * ls);
LOCAL void emptyBinding (BINDING * env); // TODO remove


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    clearValue
 *  Description:    clear a VALUE with c subvalues
 * =====================================================================================
 */
LOCAL void clearValue(int c,void * s,VALUE * p,...){

    va_list arguments;
    va_start(arguments, p); 
    N(freeValue)(p);

    for(int i = 1; i < c; ++i ){
        N(freeValue)(va_arg(arguments,VALUE *));
    }

    free(s); 
    va_end(arguments);
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    clearValuels
 *  Description:    clear VALUE with a list of c subvalues 
 *                  TODO merge with other ?
 * =====================================================================================
 */
LOCAL void clearValuels(int c,void * s,VALUE * ls){

    for(int i = 0 ; i < c ; i++){
       N(freeValue)(&ls[i]);
    }

    if(ls != NULL) free(ls);
    free(s);
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    emptyBinding
 *  Description:    clear an environment -> this part of garbage collection as
 *                      -> The contents of an environment form direct or indirect
 *                      references to language descriptors
 * =====================================================================================
 */
LOCAL void emptyBinding(BINDING * env){

    N(Binding) *node = env;
    N(Binding) *nnode = node;

    while(node) {
        node = node->next;
        free(nnode); // TODO fix this massive leak
        nnode = node;
    }
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    freeValue
 *  Description:    free a value : delete both its substructures and itself
 *                  -> to be used by the garbage collector, do not freevalues
 *                  statically !
 * =====================================================================================
 */
FUNCTIONALITY void N(freeValue)(VALUE * par){

    
    switch(par->tt){

        case N(VOID):
            return;
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
            N(freeValue)(&par->l->body);
            return clearValuels(par->l->nargs,par->l,par->l->arguments);
        }
    
        case N(PRIM) :
            return clearValuels(par->p->nargs,par->p,par->p->arguments);

        case N(SYMBOL) :
            return free(par->s);

        case N(APPLICATION) :
            return clearValuels(par->a->nargs,par->a,par->a->arguments);

        case N(IF) :
            return clearValue(3,par->f,&par->f->cond,&par->f->cons,&par->f->alt);
    
        case N(CLOSURE) : {
            emptyBinding(par->c->env);
            free(par->c->env);
            return clearValue(1,par->c,&(par->c->lambda));
        }

        case N(CONTINUATION) :
            return free(par->k);
    
        case N(CALLCC) :
            return clearValue(1,par->cc,&par->cc->function);     
    
        case N(SET) :
            return clearValue(2,par->sv,&par->sv->var,&par->sv->value);
    
        case N(LET) :
            return clearValue(3,par->lt,&par->lt->var,&par->lt->expr,&par->lt->body);
    
        case N(LETREC) :{
            N(freeValue)(&par->lr->body);
            for(int i = 0; i < par->lr->nargs; i++){
                N(freeValue)(&par->lr->vars[i]);
                N(freeValue)(&par->lr->exprs[i]);
            }
            free(par->lr->vars);
            free(par->lr->exprs);
            return free(par->lr);
        }

        case N(BEGIN) :
            return clearValuels(par->bg->nargs,par->bg,par->bg->stmts);
    
        case N(CAR) :
            return clearValue(1,par->car,&par->car->arg); 
    
        case N(CDR) :
        return clearValue(1,par->cdr,&par->cdr->arg); 
    
        case N(CONS) :
            return clearValue(2,par->cons,&par->cons->arg,&par->cons->arg2); 
    
        case N(LIST) :
            return clearValuels(par->ls->nargs,par->ls,par->ls->args);
    
        case N(QUOTE) :
            return clearValue(1,par->q,&par->q->arg); 
    
        case N(PAIRQ) :
            return clearValue(1,par->pq,&par->pq->arg); 
    
        case N(LISTQ) :
            return clearValue(1,par->lq,&par->lq->arg); 

        case N(NULLQ) :
            return clearValue(1,par->nq,&par->nq->arg); 

        case N(DEFINE) :
            return clearValue(2,par->d,&par->d->var,&par->d->expr);

        default :
            DEBUG_PRINT(("Could not clear VALUE !!!")) 
            return;
}}


#endif

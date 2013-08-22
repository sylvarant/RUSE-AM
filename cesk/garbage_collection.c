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

#ifndef ENVIRONMENT_INCLUDED
#include "binding.h" 
#endif

/*-----------------------------------------------------------------------------
 *  Local functions
 *-----------------------------------------------------------------------------*/

LOCAL void clearValuels(int c,void * s,VALUE * ls);
//LOCAL void emptyBinding (BINDING * env); // TODO remove


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    clearValuels
 *  Description:    clear VALUE with a list of c subvalues 
 * =====================================================================================
 */
LOCAL void clearValuels(int c,void * s,VALUE * ls){

    for(int i = 0 ; i < c ; i++){
       N(freeValue)(ls[i]);
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
/*LOCAL void emptyBinding(BINDING * env){

    N(Binding) *node = env;
    N(Binding) *nnode = node;

    while(node) {
        node = node->next;
        free(nnode); // TODO fix this massive leak
        nnode = node;
    }
}*/


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    freeValue
 *  Description:    free a value : delete both its substructures and itself
 *                  -> to be used by the garbage collector, do not freevalues
 *                  statically !
 * =====================================================================================
 */
FUNCTIONALITY void N(freeValue)(VALUE par){

    
    switch(par.tt){

        case N(NOP):
        case N(UNIT):
            return;

        default : break;
    }

    switch(par.b->t){
    
        case N(INT) :  
            return free(par.z);
    
        #ifdef SECURE
        case SI : 
            return free(par.i);
        #endif

        // Booleans are optimized into a static true & false presence
        case N(BOOLEAN) : return; 

        case N(LAM) : {
            N(freeValue)(par.l->body);
            return clearValuels(par.l->nargs,par.l,par.l->arguments);
        }
    
        case N(PRIM) :
            return clearValuels(par.p->nargs,par.p,par.p->arguments);

        case N(SYMBOL) :
            return free(par.s);

        case N(APPLICATION) :
            return clearValuels(par.a->nargs,par.a,par.a->arguments);

        case N(IF) :{
			VALUE * ls = MALLOC(3 * sizeof(VALUE));
			ls[0] = par.f->cond;
			ls[1] = par.f->cons;
			ls[2] = par.f->alt;
            return clearValuels(3,par.f,ls);
			}
    
        case N(CLOSURE) : {
            //emptyBinding(par.c->env); // TODO fix
            //free(par.c->env);
			VALUE * ls = MALLOC(1 * sizeof(VALUE));
			ls[0] = par.c->lambda;
            return clearValuels(1,par.c,ls);
        }

        case N(CONTINUATION) :
            return free(par.k);
    
        case N(CALLCC) : {
			VALUE * ls = MALLOC(1 * sizeof(VALUE));
			ls[0] = par.cc->function;
            return clearValuels(1,par.cc,ls);     
		}
    
        case N(SET) : {
			VALUE * ls = MALLOC(2 * sizeof(VALUE));
			ls[0] = par.sv->var;
			ls[1] = par.sv->value;
            return clearValuels(2,par.sv,ls);
		}
    
        case N(LET) : {
			VALUE * ls = MALLOC(3 * sizeof(VALUE));
			ls[0] = par.lt->var;
			ls[1] = par.lt->expr;
			ls[2] = par.lt->body;
            return clearValuels(3,par.lt,ls);
		}
    
        case N(LETREC) :{
            N(freeValue)(par.lr->body);
            for(int i = 0; i < par.lr->nargs; i++){
                N(freeValue)(par.lr->vars[i]);
                N(freeValue)(par.lr->exprs[i]);
            }
            free(par.lr->vars);
            free(par.lr->exprs);
            return free(par.lr);
        }

        case N(BEGIN) :
            return clearValuels(par.bg->nargs,par.bg,par.bg->stmts);
    
        case N(CAR) : {
			VALUE * ls = MALLOC(1 * sizeof(VALUE));
			ls[0] = par.car->arg;
            return clearValuels(1,par.car,ls); 
		}
    
        case N(CDR) : {
			VALUE * ls = MALLOC(1 * sizeof(VALUE));
			ls[0] = par.cdr->arg;
			return clearValuels(1,par.cdr,ls); 
		} 

        case N(CONS) : {
			VALUE * ls = MALLOC(2 * sizeof(VALUE));
			ls[0] = par.cons->arg;
			ls[1] = par.cons->arg2;
            return clearValuels(2,par.cons,ls);
		}
    
        case N(LIST) :
            return clearValuels(par.ls->nargs,par.ls,par.ls->args);
    
        case N(QUOTE) : {
			VALUE * ls = MALLOC(1 * sizeof(VALUE));
			ls[0] = par.q->arg;
            return clearValuels(1,par.q,ls);
		}
    
        case N(PAIRQ) : {
			VALUE * ls = MALLOC(1 * sizeof(VALUE));
			ls[0] = par.pq->arg;
            return clearValuels(1,par.pq,ls);
		}
    
        case N(LISTQ) : {
			VALUE * ls = MALLOC(1 * sizeof(VALUE));
			ls[0] = par.lq->arg;
            return clearValuels(1,par.lq,ls);
		}

        case N(NULLQ) : {
			VALUE * ls = MALLOC(1 * sizeof(VALUE));
			ls[0] = par.nq->arg;
            return clearValuels(1,par.nq,ls);
		}

        case N(DEFINE) : {
			VALUE * ls = MALLOC(2 * sizeof(VALUE));
			ls[0] = par.d->var;
			ls[1] = par.d->expr;
            return clearValuels(2,par.d,ls);
		}

        default :
            DEBUG_PRINT("Could not clear VALUE !!!"); 
            return;
}}


#endif

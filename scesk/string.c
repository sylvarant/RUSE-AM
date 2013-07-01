/*
 * =====================================================================================
 *
 *       Filename:  string.c
 *
 *    Description:  To string all CESK descriptors
 *
 *        Created:  06/27/2013 15:03:27
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#include "string.h"
#ifdef STRING_INCLUDED

// TODO remove ?
#include <stdarg.h>
#include <math.h> 
#include <string.h>

/*-----------------------------------------------------------------------------
 *  Local functions
 *-----------------------------------------------------------------------------*/

LOCAL char * generatestring (char * start,int c,VALUE v,...);
LOCAL char * generateseqstring (char * start,int c,VALUE * ls,char * del);


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    generatestring
 *  Description:    Converts a VALUE with c number of subvalues into a string 
 *                  The type of VALUE is given in the start argument
 * =====================================================================================
 */
LOCAL char * generatestring (char * start,int c,VALUE v,...){

    va_list arguments;
    va_start(arguments, v); 

    int sstart = strlen(start);
    char ** list = MALLOC(c*(sizeof(char *)));
    list[0] = N(tostring)(v,false);
    sstart   += strlen(list[0]);

    for(int i = 1; i < c; ++i ){
        list[i]   = N(tostring)(va_arg(arguments,VALUE),false);
        sstart   += strlen(list[i]);
    }

    char * str = malloc(sizeof(char) * (sstart+(c-1)+2));
    str[0] ='\0';
    strcat(str,start);

    for(int i = 0; i <c ; ++i){
        strcat(str,list[i]);     
        if(i == c-1) {}
        else{ strcat(str,",");}
    }

    strcat(str,")");

    for(int i = 0; i < c ; i++){
        free(list[i]);
    }

    free(list);
    va_end(arguments);

    return str;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    generateseqstring
 *  Description:    Converts a VALUE with c number of subvalues into a string 
 *                  The type of VALUE is given in the start argument
 *                  ! The difference with generatestring is that this one does not use 
 *                      VA_ARGS :: TODO merge both functions ?
 * =====================================================================================
 */
LOCAL char * generateseqstring(char * start,int c,VALUE * ls,char * del){

    int sstart = strlen(start);

    if(c > 0){
        char ** args =  MALLOC(c * (sizeof (char *)));

        for(int i = 0; i < c; i++){
            args[i] = N(tostring)(ls[i],false);
            sstart  += strlen(args[i]);
        }

        sstart += ((c-1) * strlen(del)) + 2; 
        char * str = MALLOC(sizeof(char) * (sstart));
        str[0] ='\0';
        strcat(str,start);

        for(int i = 0; i < c; i++){
            strcat(str,args[i]);
            if(! (i+1 == c)) strcat(str,del);
        }

        strcat(str,")");

        for(int i = 0; i < c; i++){
            free(args[i]);
        }

        free(args);

        return str;
    } 

    char * str =  MALLOC(sizeof(char) * (sstart+2));
    str[0] ='\0';
    strcat(str,start);
    strcat(str,")"); 

    return str;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    tostring
 *  Description:    convert a VALUE into string, used to debug
 * =====================================================================================
 */
FUNCTIONALITY char * N(tostring) (VALUE par,bool outer){
    
    // Filter out VALUE with no contents
    switch(par.tt){ 
    
        case N(VOID) : {
            char * str = MALLOC(5 * sizeof(char));
            str[0] = '\0';
            strcat(str,"void");
            return str;
        }
        case N(UNDEF) : {
            char * str = MALLOC (6 * sizeof(char));
            str[0] = '\0';
            strcat(str,"undef");
            return str;
        }

    }

    switch(par.b->t){

        case N(INT) : {
            int stringsize = 2;
            if(par.z->value >= 1) stringsize = ((int)log10(par.z->value) + 2); 
            char * str =  MALLOC(sizeof(char) * stringsize);
            sprintf(str,"%d",par.z->value);
            return str;
        }

        case N(BOOLEAN) : {
            char * str = MALLOC(3 * sizeof(char));
            sprintf(str,"%s",par.b->value ? "#t" : "#f");
            return str;
        }
     
        case N(SYMBOL) : {
            char * str  =  MALLOC(sizeof(char) * (strlen(par.s->name) + 1 + (outer ? 1 : 0)));
            if(outer) strcat(str,"'"); else str[0] = '\0';
            strcat(str,par.s->name);
            return str;
        }

        #ifdef SECURE
        case SI : {
            char * si = "(SI "; 
            char * cont = OTHERN(tostring)(par.i->arg,false);
            int start = strlen(si)+3+strlen(cont);
            char * str = MALLOC(sizeof(char) * start); 
            sprintf(str,"%s%s)",si,cont);
            return str;
        }
        #endif

        case N(LAM) : {
            char * start = "(λ ";
            int sstart = strlen(start);
            char * body = N(tostring)(par.l->body,false);  
            int sbody    = strlen(body);
            char ** args = MALLOC(par.l->nargs * (sizeof (char *)));
            int sargs    =  0;

            for(int i = 0; i < par.l->nargs; i++){
                args[i] = N(tostring)(par.l->arguments[i],false);
                sargs  += strlen(args[i]);
            }

            char * str = (char *) malloc(sizeof(char) * (sstart+sargs+(par.l->nargs -1) + 4 + sbody+ 2));
            str[0] ='\0';
            strcat(str,start);

            for(int i = 0; i < par.l->nargs; i++){
                strcat(str,args[i]);
                if(! (i+1 == par.l->nargs)) strcat(str,",");
            }

            strcat(str," in ");
            strcat(str,body);
            strcat(str,")");
            free(body);

            for(int i = 0; i < par.l->nargs; i++){
                free(args[i]);
            }

            free(args);
            return str;
        }

        case N(PRIM) : 
            if(par.p->exec == N(prim_sum)) return generateseqstring("(+ ",par.p->nargs,par.p->arguments," "); 
            else if(par.p->exec == N(prim_product)) return generateseqstring("(* ",par.p->nargs,par.p->arguments," "); 
            else if(par.p->exec == N(prim_difference)) return generateseqstring("(- ",par.p->nargs,par.p->arguments," "); 
            else if(par.p->exec == N(prim_numEqual)) return generateseqstring("(= ",par.p->nargs,par.p->arguments," "); 
        

        case N(APPLICATION) : 
            return generateseqstring("(-> ",par.a->nargs,par.a->arguments," ");

        case N(IF) : 
            return generatestring("(if ",3,par.f->cond,par.f->cons,par.f->alt);

        case N(CLOSURE) : 
            return generatestring("#clo(",1,par.c->lambda);
    
        case N(CONTINUATION) : {
            char * str = (char *) MALLOC(5 * sizeof(char));
            char * insert;
            if(par.k->kstar.empty != NULL){

                switch(par.k->kstar.l->t){

                    case N(KLET) : insert = "LETC";
                        break;

                    case N(KRET) : insert = "RETC";
                        break;

                    case N(KCONTINUE) : insert = "CONT";
                        break;

                    default :
                        insert = "ERRO";
                }
            }
            else{ 
                insert = "NULL";
            }
            sprintf(str,"%s",insert);
            return str;
        }
    
        case N(CALLCC) : 
            return generatestring("(call/cc ",1,par.cc->function);
    
        case N(SET) : 
            return generatestring("(set ",2,par.sv->var,par.sv->value);
    
        case N(LET) :
            return generatestring("(let ",3,par.lt->var,par.lt->expr,par.lt->body);
    
        case N(LETREC) : {
            char * start = "letrec([";
            int sstart = strlen(start);
            char * body = N(tostring)(par.lr->body,false);  
            sstart   += strlen(body);
            char ** args = MALLOC(par.lr->nargs * (sizeof (char *)));
            char ** args2 = MALLOC (par.lr->nargs * (sizeof (char *)));

            for(int i = 0; i < par.lr->nargs; i++){
                args[i] = N(tostring)(par.lr->vars[i],false);
                args2[i] = N(tostring)(par.lr->exprs[i],false);
                sstart  += strlen(args[i]) + strlen(args2[i]);
            }

            sstart += (par.lr->nargs * 3) + (par.lr->nargs -1);
            char * str = MALLOC(sizeof(char) * (sstart + 7));
            str[0] ='\0';
            strcat(str,start);

            for(int i = 0; i < par.lr->nargs; i++){
                strcat(str,"(");
                strcat(str,args[i]);
                strcat(str,",");
                strcat(str,args2[i]);
                strcat(str,")");

                if(! (i+1 == par.lr->nargs)) strcat(str,",");
            }

            strcat(str,"] in ");
            strcat(str,body);
            strcat(str,")");
            free(body);

            for(int i = 0; i < par.a->nargs; i++){
                free(args[i]);
                free(args2[i]);
            }

            free(args);
            free(args2);

            return str;
        }

        case N(BEGIN) :
            return generateseqstring("(begin ",par.bg->nargs,par.bg->stmts," ");
    
        case N(CAR) :
            return generatestring("(car ",1,par.car->arg);
    
        case N(CDR) :
            return generatestring("(cdr ",1,par.cdr->arg);
    
        case N(CONS) :
            return generatestring("(cons ",2,par.cons->arg,par.cons->arg2);
    
        case N(LIST) : 
            if(par.ls->islist)  return generateseqstring((outer ? "'(" : "("),par.ls->nargs,par.ls->args," ");
            return generateseqstring((outer ? "'(" : "("),par.ls->nargs,par.ls->args," . ");
    
        case N(QUOTE) :
            return generatestring("(quote ",1,par.q->arg);
   
        case N(PAIRQ) :
            return generatestring("(pair? ",1,par.pq->arg);
    
        case N(LISTQ) :
            return generatestring("(list? ",1,par.lq->arg);

        case N(NULLQ) :
            return generatestring("(null? ",1,par.nq->arg);

        case N(DEFINE) :
            return generatestring("(define ",2,par.d->var,par.d->expr);

        default :
            DEBUG_PRINT(("Could not convert Value to string!!!")) 
            return NULL;
    }
}

#endif

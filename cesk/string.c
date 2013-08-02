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

#include <string.h>

#ifdef FPGA
    #include "uart.h"
#endif

#ifdef SECURE // hack
HOOK char * OTHERN(toString)(OTHERVALUE,unsigned int);
#endif

/*-----------------------------------------------------------------------------
 *  Local functions
 *-----------------------------------------------------------------------------*/

LOCAL unsigned int nchar(int v);
LOCAL char * generateseqString (char * start,int c,VALUE * ls,char * del);


/*-----------------------------------------------------------------------------
 *  Custom IO on Sancus
 *-----------------------------------------------------------------------------*/
#ifdef SANCUS_SPM
int putchar(int c)
{
    #ifdef FPGA
    if(c == '\n'){
        uart2_write_byte('\r');
        uart2_write_byte(c);
    #else
    P1OUT  = c;
    P1OUT |= 0x80;
    #endif
    return c;
}
 #endif

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    nchar
 *  Description:    returns the number of characters that make up an integer
 * =====================================================================================
 */
LOCAL unsigned int nchar(int v){

	int i = 0;

	do{
		i++;
		v /= 10;
	}while(v);

	return i;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    generateseqString
 *  Description:    Converts a VALUE with c number of subvalues into a string 
 *                  The type of VALUE is given in the start argument
 *                  ! The difference with generatestring is that this one does not use 
 * =====================================================================================
 */
LOCAL char * generateseqString(char * start,int c,VALUE * ls,char * del){

    int sstart = strlen(start);

    if(c > 0){
        char ** args =  MALLOC(c * (sizeof (char *)));

        for(int i = 0; i < c; i++){
            args[i] = N(toString)(ls[i],0);
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
 *         Name:    toString
 *  Description:    convert a VALUE into string, used to debug
 * =====================================================================================
 */
FUNCTIONALITY char * N(toString) (VALUE par,unsigned int outer){
    
    // Filter out VALUE with no contents
    switch(par.tt){ 

        case N(NOP) : {
            char * str = MALLOC(4 * sizeof(char));
            str[0] = '\0';
            strcat(str,"nop");
            return str;
        }
    
        case N(UNIT) : {
            char * str = MALLOC(5 * sizeof(char));
            str[0] = '\0';
            strcat(str,"void");
            return str;
        }

        default : break;

    }

    switch(par.b->t){

        case N(INT) : {
            int stringsize = 2;
            if(par.z->value >= 1) stringsize = (nchar(par.z->value) + 2); 
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
            char * cont = OTHERN(toString)(par.i->arg,0);
            int start = strlen(si)+3+strlen(cont);
            char * str = MALLOC(sizeof(char) * start); 
            sprintf(str,"%s%s)",si,cont);
            return str;
        }
        #else
        case IS : {
            char * start = "(IS ";
            int sstart = strlen(start);
            int stringsize = 4;
            if(par.i->label >= 1) stringsize = (nchar(par.i->label) + 4);   
            char * str = (char *) malloc(sizeof(char) * (stringsize + sstart));
            sprintf(str,"%s%d)",start,par.i->label);
            return str;
        }
        #endif

        case N(LAM) : {
            char * start = "(λ ";
            int sstart = strlen(start);
            char * body = N(toString)(par.l->body,0);  
            int sbody    = strlen(body);
            char ** args = MALLOC(par.l->nargs * (sizeof (char *)));
            int sargs    =  0;

            for(int i = 0; i < par.l->nargs; i++){
                args[i] = N(toString)(par.l->arguments[i],0);
                sargs  += strlen(args[i]);
            }

            char * str = MALLOC(sizeof(char) * (sstart+sargs+(par.l->nargs -1) + 4 + sbody+ 2));
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
            if(par.p->exec == N(sumPrim)) return generateseqString("(+ ",par.p->nargs,par.p->arguments," "); 
            else if(par.p->exec == N(productPrim)) return generateseqString("(* ",par.p->nargs,par.p->arguments," "); 
            else if(par.p->exec == N(differencePrim)) return generateseqString("(- ",par.p->nargs,par.p->arguments," "); 
            else if(par.p->exec == N(numequalPrim)) return generateseqString("(= ",par.p->nargs,par.p->arguments," "); 
        

        case N(APPLICATION) : 
            return generateseqString("(-> ",par.a->nargs,par.a->arguments," ");

        case N(IF) : { 
			VALUE * ls = MALLOC(3 * sizeof(VALUE));
			ls[0] = par.f->cond;
			ls[1] = par.f->cons;
			ls[2] = par.f->alt;
            char * str = generateseqString("(if ",3,ls,",");
            free(ls);
            return str;
        }

        case N(CLOSURE) : {
			VALUE * ls = MALLOC(1 * sizeof(VALUE));
			ls[0] = par.c->lambda;
            char * str = generateseqString("#clo(",1,ls,",");
            free(ls);
            return str;
        }
    
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
    
        case N(CALLCC) : {
			VALUE * ls = MALLOC(1 * sizeof(VALUE));
			ls[0] = par.cc->function;
            char * str = generateseqString("(call/cc ",1,ls,",");
            free(ls);
            return str;
        }
    
        case N(SET) : {
			VALUE * ls = MALLOC(2 * sizeof(VALUE));
			ls[0] = par.sv->var;
			ls[1] = par.sv->value;
            char * str = generateseqString("(set ",2,ls,",");
            free(ls);
            return str;
        }
    
        case N(LET) : {
			VALUE * ls = MALLOC(3 * sizeof(VALUE));
			ls[0] = par.lt->var;
			ls[1] = par.lt->expr;
			ls[2] = par.lt->body;
            char * str = generateseqString("(let ",3,ls,",");
            free(ls);
            return str;
        }
    
        case N(LETREC) : {
            char * start = "letrec([";
            int sstart = strlen(start);
            char * body = N(toString)(par.lr->body,0);  
            sstart   += strlen(body);
            char ** args = MALLOC(par.lr->nargs * (sizeof (char *)));
            char ** args2 = MALLOC (par.lr->nargs * (sizeof (char *)));

            for(int i = 0; i < par.lr->nargs; i++){
                args[i] = N(toString)(par.lr->vars[i],0);
                args2[i] = N(toString)(par.lr->exprs[i],0);
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
            return generateseqString("(begin ",par.bg->nargs,par.bg->stmts," ");
    
        case N(CAR) : {
			VALUE * ls = MALLOC(1 * sizeof(VALUE));
			ls[0] = par.car->arg;
            char * str = generateseqString("(car ",1,ls,",");
            free(ls);
            return str;
        }
    
        case N(CDR) : {
			VALUE * ls = MALLOC(1 * sizeof(VALUE));
			ls[0] = par.cdr->arg;
            char * str = generateseqString("(cdr ",1,ls,",");
            free(ls);
            return str;
        }
    
        case N(CONS) : {
			VALUE * ls = MALLOC(2 * sizeof(VALUE));
			ls[0] = par.cons->arg;
			ls[1] = par.cons->arg2;
            char * str = generateseqString("(cons ",2,ls,",");
            free(ls);
            return str;
        }
    
        case N(LIST) : 
            if(par.ls->islist)  return generateseqString((outer ? "'(" : "("),par.ls->nargs,par.ls->args," ");
            return generateseqString((outer ? "'(" : "("),par.ls->nargs,par.ls->args," . ");
    
        case N(QUOTE) : {
			VALUE * ls = MALLOC(1 * sizeof(VALUE));
			ls[0] = par.q->arg;
            char * str = generateseqString("(quote ",1,ls,",");
            free(ls);
            return str;
        }
   
        case N(PAIRQ) : {
            VALUE * ls = MALLOC(1* sizeof(VALUE));
            ls[0] = par.pq->arg;
            char * str =generateseqString("(pair? ",1,ls,",");
            free(ls);
            return str;
        }
    
        case N(LISTQ) : {
            VALUE * ls = MALLOC(1 * sizeof(VALUE));
            ls[0] = par.lq->arg;
            char * str = generateseqString("(list? ",1,ls,",");
            free(ls);
            return str;
        }

        case N(NULLQ) : {
            VALUE * ls = MALLOC(1 * sizeof(VALUE));
            ls[0] = par.lq->arg;
            char * str = generateseqString("(null? ",1,ls,",");
            free(ls);
            return str;
       } 

        case N(DEFINE) : {
            VALUE * ls = MALLOC(2 * sizeof(VALUE));
            ls[0] = par.d->var;
            ls[1] = par.d->expr;
            char * str = generateseqString("(define ",2,ls,",");
            free(ls);
            return str;
        }

        default :
            DEBUG_PRINT("Could not convert Value to string!!!"); 
            return NULL;
    }
}

#endif

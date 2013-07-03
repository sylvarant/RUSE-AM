/*
 * =====================================================================================
 *
 *       Filename:  scheme.c
 *
 *    Description:  The scheme language in c
 *
 *        Version:  1.0
 *        Created:  10/17/2012 14:54:40
 *       Compiler:  gcc
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

/*-----------------------------------------------------------------------------
 *  Preprocessor
 *-----------------------------------------------------------------------------*/
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdarg.h>
#include <math.h>
#include "scheme.h"

#define MAKE_(TYPE,TAG,ID,ARG) extern Value make##TYPE(Value v){\
    Value val;\
    val.ID      = (struct TYPE *) malloc(sizeof(struct TYPE));\
    val.ID->t       = TAG;\
    val.ID->ARG = v; \
    return val;\
}

#define COPY_(TYPE,TAG,ID,ARG,AA){\
    Value out;\
    out.ID = (struct TYPE *) malloc(sizeof(struct TYPE));\
    out.ID->t     = TAG;\
    out.ID->nargs = ARG.ID->nargs;\
    out.ID->AA = (Value *) malloc( ARG.ID->nargs * (sizeof (Value)));\
    for(int i = 0; i < ARG.ID->nargs; i++){\
            out.ID->AA[i] = copyvalue(ARG.ID->AA[i]);}\
    return out;}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    generatestring
 *  Description:    do internal work
 * =====================================================================================
 */
static char * generatestring(char * start,int c,Value v,...){
    va_list arguments;
    va_start(arguments, v); 

    int sstart = strlen(start);
    char ** list = (char **) malloc(c*(sizeof(char *)));

    list[0] = toString(v,false);
    sstart   += strlen(list[0]);

    for(int i = 1; i < c; ++i ){
        list[i]   = toString(va_arg(arguments,Value),false);
        sstart   += strlen(list[i]);
    }

    char * str = (char *) malloc(sizeof(char) * (sstart+(c-1)+2));
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
 *  Description:    do internal work
 * =====================================================================================
 */
static char * generateseqstring(char * start,int c,Value * ls,char * del){
    int sstart = strlen(start);
    if(c > 0){
        char ** args = (char **) malloc(c * (sizeof (char *)));
        for(int i = 0; i < c; i++){
            args[i] = toString(ls[i],false);
            sstart  += strlen(args[i]);
        }
        sstart += ((c-1) * strlen(del)) + 2; 
        char * str = (char *) malloc(sizeof(char) * (sstart));
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

    char * str = (char *) malloc(sizeof(char) * (sstart+2));
    str[0] ='\0';
    strcat(str,start);
    strcat(str,")"); 

    return str;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    toString
 *  Description:    conver Value into string, used to debug
 * =====================================================================================
 */
extern char * toString (Value par,bool outer){
    
    switch(par.tt){ 
    
        case VOID : {
            char * str = (char *) malloc(5 * sizeof(char));
            str[0] = '\0';
            strcat(str,"void");
            return str;
        }
        case UNDEF : {
            char * str = (char *) malloc(6 * sizeof(char));
            str[0] = '\0';
            strcat(str,"undef");
            return str;
        }

    }

    switch(par.b->t){

    case INT : {
        int stringsize = 2;
        if(par.z->value >= 1) stringsize = ((int)log10(par.z->value) + 2);   
        char * str = (char *) malloc(sizeof(char) * stringsize);
        sprintf(str,"%d",par.z->value);
        return str;
    }

    case BOOLEAN : {
        char * str = (char *) malloc(3 * sizeof(char));
        sprintf(str,"%s",par.b->value ? "#t" : "#f");
        return str;
    }
     
    case SYMBOL : {
        char * str  =(char *) malloc(sizeof(char) * (strlen(par.s->name) + 1 + (outer ? 1 : 0)));
        if(outer) strcat(str,"'"); else str[0] = '\0';
        strcat(str,par.s->name);
        return str;
    }

    case IS : {
        char * start = "(IS ";
        int sstart = strlen(start);
        int stringsize = 4;
        if(par.z->value >= 1) stringsize = ((int)log10(par.z->value) + 4);   
        char * str = (char *) malloc(sizeof(char) * (stringsize + sstart));
        sprintf(str,"%s%d)",start,par.z->value);
        return str;
    }

    case LAM : {
        char * start = "(λ ";
        int sstart = strlen(start);
        char * body = toString(par.l->body,false);  
        int sbody    = strlen(body);
        char ** args = (char **) malloc(par.l->nargs * (sizeof (char *)));
        int sargs    =  0;
        for(int i = 0; i < par.l->nargs; i++){
            args[i] = toString(par.l->arguments[i],false);
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

    case PRIM : 
        if(par.p->exec == sumPrim) 
            return generateseqstring("(+ ",par.p->nargs,par.p->arguments," "); 
        else if(par.p->exec == productPrim)
            return generateseqstring("(* ",par.p->nargs,par.p->arguments," "); 
        else if(par.p->exec == differencePrim)
            return generateseqstring("(- ",par.p->nargs,par.p->arguments," "); 
        else if(par.p->exec == numequalPrim)
            return generateseqstring("(= ",par.p->nargs,par.p->arguments," "); 

    case APPLICATION : 
        return generateseqstring("(-> ",par.a->nargs,par.a->arguments," ");

    case IF :
        return generatestring("(if ",3,par.f->cond,par.f->cons,par.f->alt);
    
    case CLOSURE :
        return generatestring("#clo(",1,par.c->lambda);
    
    case CONTINUATION :{
        char * str = (char *) malloc(5 * sizeof(char));
        char * insert;
        if(par.k->kstar.empty != NULL){
        switch(par.k->kstar.l->t){
            case KLET :
                insert = "LETC";
                break;
            case KRET :
                insert = "RETC";
                break;
            case KCONTINUE:
                insert = "CONT";

            default :
                insert = "ERRO";
        }}else{ insert = "NULL";}
        sprintf(str,"%s",insert);
        return str;
     }
    
    case CALLCC :
        return generatestring("(call/cc ",1,par.cc->function);
    
    case  SET :
        return generatestring("(set ",2,par.sv->var,par.sv->value);
    
    case LET :
        return generatestring("(let ",3,par.lt->var,par.lt->expr,par.lt->body);
    
    case LETREC : {
        char * start = "letrec([";
        int sstart = strlen(start);
        char * body = toString(par.lr->body,false);  
        sstart   += strlen(body);
        char ** args = (char **) malloc(par.lr->nargs * (sizeof (char *)));
        char ** args2 = (char **) malloc(par.lr->nargs * (sizeof (char *)));
        for(int i = 0; i < par.lr->nargs; i++){
            args[i] = toString(par.lr->vars[i],false);
            args2[i] = toString(par.lr->exprs[i],false);
            sstart  += strlen(args[i]) + strlen(args2[i]);
        }
        sstart += (par.lr->nargs * 3) + (par.lr->nargs -1);
        char * str = (char *) malloc(sizeof(char) * (sstart + 7));
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
    case BEGIN :
        return generateseqstring("(begin ",par.bg->nargs,par.bg->stmts," ");
    
    case CAR :
        return generatestring("(car ",1,par.car->arg);
    
    case CDR :
        return generatestring("(cdr ",1,par.cdr->arg);
    
    case CONS :
        return generatestring("(cons ",2,par.cons->arg,par.cons->arg2);
    
    case LIST : 
        if(par.ls->islist) 
            return generateseqstring((outer ? "'(" : "("),par.ls->nargs,par.ls->args," ");
        return generateseqstring((outer ? "'(" : "("),par.ls->nargs,par.ls->args," . ");
    
    case QUOTE :
        return generatestring("(quote ",1,par.q->arg);
   
    case PAIRQ :
        return generatestring("(pair? ",1,par.pq->arg);
    
    case LISTQ :
        return generatestring("(list? ",1,par.lq->arg);

    case NULLQ :
        return generatestring("(null? ",1,par.nq->arg);

    case DEFINE :
        return generatestring("(define ",2,par.d->var,par.d->expr);

    default :
        DEBUG_PRINT(("Could not convert Value to string!!!")) 
        return NULL;
}}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    clearvalue
 *  Description:    clear a certain type value
 * =====================================================================================
 */
static void clearvalue(int c,void * s,Value * p,...){

    va_list arguments;
    va_start(arguments, p); 

    freevalue(p);
    for(int i = 1; i < c; ++i ){
        freevalue(va_arg(arguments,Value *));
    }
    free(s); 
    va_end(arguments);
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    clearvalue*
 *  Description:    clear Values made of sequences
 * =====================================================================================
 */
static void clearvaluels(int c,void * s,Value * ls){
    for(int i = 0 ; i < c ; i++){
       freevalue(&ls[i]);
    }
    if(ls != NULL) free(ls);
    free(s);
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    freevalue
 *  Description:    free a value
 * =====================================================================================
 */
extern void freevalue(Value * par){

    switch(par->tt){
        case VOID:
        case UNDEF:
            return;
    }

    switch(par->b->t){
    
    case INT :  
        return free(par->z);

    case IS : 
        return free(par->i);
       
    case BOOLEAN : 
        return; //free(par->b);

    case LAM :
        freevalue(&par->l->body);
        return clearvaluels(par->l->nargs,par->l,par->l->arguments);
    
    case PRIM :
        return clearvaluels(par->p->nargs,par->p,par->p->arguments);

    case SYMBOL :
        return free(par->s);

    case APPLICATION :
        return clearvaluels(par->a->nargs,par->a,par->a->arguments);

    case IF :
        return clearvalue(3,par->f,&par->f->cond,&par->f->cons,&par->f->alt);
    
    case CLOSURE :
        emptyenv(par->c->env);
        free(par->c->env);
        return clearvalue(1,par->c,&(par->c->lambda));

    case CONTINUATION :
        return free(par->k);
    
    case CALLCC :
        return clearvalue(1,par->cc,&par->cc->function);     
    
    case SET :
        return clearvalue(2,par->sv,&par->sv->var,&par->sv->value);
    
    case LET :
        return clearvalue(3,par->lt,&par->lt->var,&par->lt->expr,&par->lt->body);
    
    case LETREC :
        freevalue(&par->lr->body);
        for(int i = 0; i < par->lr->nargs; i++){
            freevalue(&par->lr->vars[i]);
            freevalue(&par->lr->exprs[i]);
        }
        free(par->lr->vars);
        free(par->lr->exprs);
        return free(par->lr);
    
    case BEGIN :
        return clearvaluels(par->bg->nargs,par->bg,par->bg->stmts);
    
    case CAR :
        return clearvalue(1,par->car,&par->car->arg); 
    
    case CDR :
        return clearvalue(1,par->cdr,&par->cdr->arg); 
    
    case CONS :
        return clearvalue(2,par->cons,&par->cons->arg,&par->cons->arg2); 
    
    case LIST :
        return clearvaluels(par->ls->nargs,par->ls,par->ls->args);
    
    case QUOTE :
        return clearvalue(1,par->q,&par->q->arg); 
    
    case PAIRQ :
        return clearvalue(1,par->pq,&par->pq->arg); 
    
    case LISTQ :
        return clearvalue(1,par->lq,&par->lq->arg); 

    case NULLQ :
        return clearvalue(1,par->nq,&par->nq->arg); 

    case DEFINE :
        return clearvalue(2,par->d,&par->d->var,&par->d->expr);

    default :
        DEBUG_PRINT(("Could not clear Value !!!")) 
        return;
    
}}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    sumPrim
 *  Description:    sum operation
 * =====================================================================================
 */
extern Value sumPrim(Value a, Value b) {
    return makeInt(a.z->value + b.z->value) ;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    productPrim
 *  Description:    product operation
 * =====================================================================================
 */
extern Value productPrim(Value a, Value b) {
    return makeInt(a.z->value * b.z->value) ;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    differencePrim
 *  Description:    difference operation
 * =====================================================================================
 */
extern Value differencePrim(Value a, Value b) {
    return makeInt(a.z->value - b.z->value) ;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    numequalPrim
 *  Description:    equal operation
 * =====================================================================================
 */
extern Value numequalPrim(Value a, Value b) {
    return makeBoolean(a.z->value == b.z->value) ;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeInt
 *  Description:    create a Value INT
 * =====================================================================================
 */
extern Value makeInt(int n) {
    Value v;
    struct Int * data = (struct Int*) malloc(sizeof(struct Int));
    v.z = data;
    v.z->t = INT ;
    v.z->value = n ;
    return v ;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeIS
 *  Description:    create a Value IS
 * =====================================================================================
 */
extern Value makeIS(int n) {
    Value v;
    struct IS * data = (struct IS*) malloc(sizeof(struct IS));
    v.i = data;
    v.i->t = IS ;
    v.i->label = n ;
    return v ;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeBoolean
 *  Description:    create a Value BOOLEAN
 * =====================================================================================
 */
extern Value makeBoolean(unsigned int b) {
    static struct Boolean datatrue  = {BOOLEAN,1};
    static struct Boolean datafalse = {BOOLEAN,0};

    Value v ;
    if(b){ v.b = &datatrue;}
    else{ v.b = &datafalse;}
    return v ;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeIf
 *  Description:    create a Value IF
 * =====================================================================================
 */
extern Value makeIf(Value a,Value b,Value c){
    Value v;
    struct If * data = (struct If*) malloc(sizeof(struct If));
    v.f = data;
    v.f->t    = IF;
    v.f->cond = a;
    v.f->cons = b;
    v.f->alt  = c;
    return v;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeLambda
 *  Description:    create a Value LAM
 * =====================================================================================
 */
extern Value makeLambda(int c,Value body,...){

    va_list arguments;
    va_start(arguments, body); 

    Value v;

    struct Lambda * data = (struct Lambda*) malloc(sizeof(struct Lambda));
    v.l = data;
    v.l->t = LAM;
    v.l->nargs = c;
    v.l->body  = body;

    Value * list = (Value *) malloc(c*(sizeof(Value)));
    for(int i = 0; i < c; ++i )
            list[i] = va_arg(arguments,Value);
    
    va_end(arguments);
    v.l->arguments = list;
    
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makePrim
 *  Description:    create a Value Prim
 * =====================================================================================
 */
extern Value makePrim(int c,PrimOp ex,Value arg,...){

    va_list arguments;
    va_start(arguments, arg); 

    Value v;
    struct Prim * data = (struct Prim*) malloc(sizeof(struct Prim));
    v.p = data;
    v.p->t = PRIM;
    v.p->exec  = ex;
    v.p->nargs = c;

    Value * list = (Value *) malloc(c*(sizeof(Value)));
    list[0] = arg;
    for(int i = 1; i < c; ++i )
            list[i] = va_arg(arguments,Value);

    va_end(arguments);
    v.p->arguments = list;

    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeApplication
 *  Description:    create a Value APPLICATION
 * =====================================================================================
 */
extern Value makeApplication(int c,Value a,...){
    
    va_list arguments;
    va_start(arguments, a); 

    Value v;
    struct Application * data = (struct Application*) malloc(sizeof(struct Application));
    v.a = data;
    v.a->t         = APPLICATION;
    v.a->nargs = c;
    
    Value * list = (Value *) malloc(c*(sizeof(Value)));
    list[0] = a; 
    for(int i = 1; i < c; ++i )
            list[i] = va_arg(arguments,Value);

    va_end(arguments);
    v.a->arguments = list;

    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeSymbol
 *  Description:    create a Value SYMBOL
 * =====================================================================================
 */
extern Value makeSymbol(char * name){
    Value v;
    struct Symbol * data = (struct Symbol*) malloc(sizeof(struct Symbol));
    v.s = data;
    v.s->t    = SYMBOL;
    v.s->name = name;
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeClosure
 *  Description:    create a Closure
 * =====================================================================================
 */
extern Value makeClosure(Value atom, environ * htbl){
        Value clos;
          
        struct Closure * data = (struct Closure*) malloc(sizeof(struct Closure));
        clos.c = data;
        clos.c->t      = CLOSURE;
        clos.c->lambda = atom;
        clos.c->env = copyenv(htbl);

		return clos;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeCallcc
 *  Description:    create a Callcc
 * =====================================================================================
 */
MAKE_(Callcc,CALLCC,cc,function)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeSet
 *  Description:    create a SetV
 * =====================================================================================
 */
extern Value makeSet(Value v,Value t){
    Value val;

    val.sv        = (struct SetV *) malloc(sizeof(struct SetV));
    val.sv->t     = SET;
    val.sv->var   = v;
    val.sv->value = t;

    return val;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeLet
 *  Description:    create a Let
 * =====================================================================================
 */
extern Value makeLet(Value v,Value t,Value b){
    Value val;

    val.lt       = (struct Let *) malloc(sizeof(struct Let));
    val.lt->t    = LET;
    val.lt->var  = v;
    val.lt->expr = t;
    val.lt->body = b;

    return val;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeLetrec
 *  Description:    create a Letrec
 * =====================================================================================
 */
extern Value makeLetrec(int c,Value v,Value t,...){
    va_list arguments;
    va_start(arguments, t); 

    Value val;

    val.lr        = (struct Letrec *) malloc(sizeof(struct Letrec));
    val.lr->t     = LETREC;
    val.lr->nargs = c;
    val.lr->body = v;
    
    val.lr->vars = (Value *) malloc(c*(sizeof(Value)));
    val.lr->exprs = (Value *) malloc(c*(sizeof(Value)));

    val.lr->vars[0] = t; 
    for(int i = 1; i < c; ++i ){
        val.lr->vars[i] = va_arg(arguments,Value);
    }
    for(int i = 0; i < c; ++i ){
        val.lr->exprs[i] = va_arg(arguments,Value);
    }

    va_end(arguments);

    return val;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeContinuation
 *  Description:    internal creation of continuation
 * =====================================================================================
 */
extern Value makeContinuation(kont kstar){
    Value val;
    
    val.k = (struct Continuation *) malloc(sizeof(struct Continuation));
    val.k->t     = CONTINUATION;
    val.k->kstar = kstar;
    
    return val;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeVoid
 *  Description:    return a value
 * =====================================================================================
 */
extern Value makeVoid(void){
    Value val;
    val.tt = VOID;
    return val;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeBegin
 *  Description:    return a value
 * =====================================================================================
 */
extern Value makeBegin(int c, Value args,...){
    va_list arguments;
    va_start(arguments, args); 

    Value v;
    struct Begin * data = (struct Begin*) malloc(sizeof(struct Begin));
    v.bg = data;
    v.bg->t         = BEGIN;
    v.bg->nargs = c;
    
    Value * list = (Value *) malloc(c*(sizeof(Value)));
    list[0] = args;
    for(int i = 1; i < c; ++i )
            list[i] = va_arg(arguments,Value);

    va_end(arguments);
    v.bg->stmts = list;

    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeCar
 *  Description:    return a value
 * =====================================================================================
 */
MAKE_(Car,CAR,car,arg)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeCdr
 *  Description:    return a value
 * =====================================================================================
 */
MAKE_(Cdr,CDR,cdr,arg)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeCons
 *  Description:    return a value
 * =====================================================================================
 */
extern Value makeCons(Value v,Value v2){
    Value val;
    
    val.cons = (struct Cons *) malloc(sizeof(struct Cons));
    val.cons->t     = CONS;
    val.cons->arg = v;
    val.cons->arg2 = v2;

    return val;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeList
 *  Description:    return a value
 * =====================================================================================
 */
extern Value makeList(int c,Value args,...){
    va_list arguments;
    va_start(arguments, args); 

    Value v;
    struct List * data = (struct List*) malloc(sizeof(struct List));
    v.ls = data;
    v.ls->t         = LIST;
    v.ls->islist = 1;
    v.ls->nargs = c;
    
    Value * list = (Value *) malloc(c*(sizeof(Value)));
    list[0] = args;
    for(int i = 1; i < c; ++i )
            list[i] = va_arg(arguments,Value);

    va_end(arguments);
    v.ls->args = list;

    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeNIL
 *  Description:    return a value
 * =====================================================================================
 */
extern Value makeNIL(void){
    Value v;
    struct List * data = (struct List*) malloc(sizeof(struct List));
    v.ls = data;
    v.ls->t         = LIST;
    v.ls->nargs = 0;
    v.ls->islist = 1;
    v.ls->args = NULL;
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makePair
 *  Description:    return a value
 * =====================================================================================
 */
extern Value makePair(Value v,Value v2){
    Value ret       = makeList(2,v,v2);
    ret.ls->islist = 0;
    return ret;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeQuote
 *  Description:    return a value
 * =====================================================================================
 */
MAKE_(Quote,QUOTE,q,arg)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makePairQ
 *  Description:    return a value
 * =====================================================================================
 */
MAKE_(PairQ,PAIRQ,pq,arg)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeListQ
 *  Description:    return a value
 * =====================================================================================
 */
MAKE_(ListQ,LISTQ,lq,arg)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeListQ
 *  Description:    return a value
 * =====================================================================================
 */
MAKE_(NullQ,NULLQ,nq,arg)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeUndef
 *  Description:    return a value
 * =====================================================================================
 */
extern Value makeUndef(void){
    Value val;
    val.tt = UNDEF;
    return val;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeDefine
 *  Description:    return a value
 * =====================================================================================
 */
extern Value makeDefine(Value v,Value v2){
    Value val;
    
    val.d = (struct Define *) malloc(sizeof(struct Define));
    val.d->t     = DEFINE;
    val.d->var = v;
    val.d->expr = v2;
    return val;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    copyvalue
 *  Description:    copy a value
 * =====================================================================================
 */
extern Value copyvalue(Value par){

    switch(par.tt){

        case VOID : 
            return makeVoid(); 

        case UNDEF :
            return makeUndef(); 
    }

    switch(par.b->t){

    case INT :
        return makeInt(par.z->value);

    case IS :
        return makeIS(par.i->label);
    
    case BOOLEAN :
        return par;
    
    case LAM :{
        Value out;
        Value body = copyvalue(par.l->body);
        out.l = (struct Lambda*) malloc(sizeof(struct Lambda));
        out.l->t            = LAM;
        out.l->body         = body;
        out.l->nargs        = par.l->nargs;
        out.l->arguments    = (Value *) malloc((out.l->nargs)*(sizeof(Value)));
        for(int i           = 0; i < par.l->nargs; i++){
            out.l->arguments[i] = copyvalue(par.l->arguments[i]);
        }
        return out;
    }

    case PRIM :{
        Value out;
        out.p            = (struct Prim*) malloc(sizeof(struct Prim));
        out.p->t         = PRIM;
        out.p->exec      = par.p->exec;
        out.p->nargs     = par.p->nargs;
        out.p->arguments = (Value *) malloc((out.p->nargs)*(sizeof(Value)));
        for(int i = 0; i < par.p->nargs; i++){
            out.p->arguments[i] = copyvalue(par.p->arguments[i]);
        }
        return out;
    }

    case SYMBOL :
        return makeSymbol(par.s->name);

    case APPLICATION : COPY_(Application,APPLICATION,a,par,arguments)

    case IF :
        return makeIf(copyvalue(par.f->cond),copyvalue(par.f->cons),copyvalue(par.f->alt));
    
    case CLOSURE :
        return makeClosure(copyvalue(par.c->lambda),par.c->env);

    case CONTINUATION :
        return makeContinuation(par.k->kstar); // TODO copy ?

    case CALLCC :
        return makeCallcc(copyvalue(par.cc->function));

    case SET :
        return makeSet(copyvalue(par.sv->var),copyvalue(par.sv->value)); 
    
    case LET :
        return makeLet(copyvalue(par.lt->var),copyvalue(par.lt->expr),copyvalue(par.lt->body));
    
    case LETREC : {
        Value out;
        out.lr        = (struct Letrec *) malloc(sizeof(struct Letrec));
        out.lr->t     = LETREC;
        out.lr->nargs = par.lr->nargs;
        out.lr->body  = copyvalue(par.lr->body);
        out.lr->vars  = (Value *) malloc((out.lr->nargs)*(sizeof(Value)));
        out.lr->exprs  = (Value *) malloc((out.lr->nargs)*(sizeof(Value)));
        for(int i = 0; i < out.lr->nargs; i++){
            out.lr->vars[i] = copyvalue(par.lr->vars[i]);
        }
        for(int i = 0; i < out.lr->nargs; i++){
            out.lr->exprs[i] = copyvalue(par.lr->exprs[i]);
        }
        return out;
    }

    case BEGIN : COPY_(Begin,BEGIN,bg,par,stmts)

    case CAR :
        return makeCar(copyvalue(par.car->arg));

    case CDR :
        return makeCdr(copyvalue(par.cdr->arg));
    
    case CONS :
        return makeCons(copyvalue(par.cons->arg),copyvalue(par.cons->arg2));

    case LIST : {
        Value out;
        out.ls            = (struct List*) malloc(sizeof(struct List));
        out.ls->t         = LIST;
        out.ls->islist  = par.ls->islist;
        out.ls->nargs   = par.ls->nargs;
        out.ls->args = (Value *) malloc((out.ls->nargs)*(sizeof(Value)));
        for(int i = 0; i < par.ls->nargs; i++){
            out.ls->args[i] = copyvalue(par.ls->args[i]);
        }
        return out;
    }


    case QUOTE :
        return makeQuote(copyvalue(par.q->arg));
    
    case PAIRQ :
        return makePairQ(copyvalue(par.pq->arg));
    
    case LISTQ :
        return makeListQ(copyvalue(par.lq->arg));

    case DEFINE :
        return makeDefine(copyvalue(par.d->var),copyvalue(par.d->expr));

    case NULLQ :
        return makeNullQ(copyvalue(par.nq->arg));

      default :
         DEBUG_PRINT(("Could not Copy !!!"))
         exit(1);
}}

/*
 * =====================================================================================
 *
 *       Filename:  cesk.c
 *
 *    Description:  Cesk machine in c
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
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "sscheme.h"
#include "string.h"
#include <stdarg.h>

#define MEM_SIZE 1024
#define NELEMS(x)  (sizeof(x) / sizeof(x[0]))
#define FREECELL(y) do{} while(0); // if(y.b != NULL) sfreevalue(&y);



/*-----------------------------------------------------------------------------
 *  Functions
 *-----------------------------------------------------------------------------*/
// functionality



// Internal magic
FUNCTIONALITY void inject (void);
// TODO incomplete


// list
FUNCTIONALITY void slinsert(sfunctions ** ls,int label);
FUNCTIONALITY bool slhas(sfunctions * ls,int label);


/*-----------------------------------------------------------------------------
 *  Types
 *-----------------------------------------------------------------------------*/

// what is what 
typedef SValue * memory;

// CESK State
typedef struct state_t{
    SValue control;
    N(environ) * environment; 
    memory storage;
    skont continuation;
    sfunctions * functions;
	int free_adr;
}state;

typedef struct answer_t{
    SValue ans;
    state * s;
}answer;

// state + answer
typedef struct limbo_t{
    state * computation;
    answer ans;
}limbo;

// secret data
SECRET_DATA state * mystate = NULL;
FUNCTIONALITY answer steprec (state * s);
FUNCTIONALITY limbo step(state * s);
FUNCTIONALITY limbo applyproc(SValue proc,SValue * args,state *s);
FUNCTIONALITY limbo applykont(SValue val,skont  k,state * s);
FUNCTIONALITY SValue evalatom(SValue atom,N(environ) * htbl,memory mem);
FUNCTIONALITY bool isatom(SValue el);

/*-----------------------------------------------------------------------------
 *  Debug
 *-----------------------------------------------------------------------------*/

#ifdef DEBUG
LOCAL void debugstate(state * s);

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    debugstate
 *  Description:    print the current cesk state
 * =====================================================================================
 */
LOCAL void debugstate(state * s){
    DEBUG_PRINT(("==========================")) 
    DEBUG_PRINT(("** CONTROL")) 

    char * ctrl = N(tostring)(s->control,false);
    DEBUG_PRINT((ctrl)) 
    free(ctrl);
    DEBUG_PRINT(("** STORES : %d",s->free_adr)) 
    for(int i = 0; i < s->free_adr; i++){
        char * str =  N(tostring)(s->storage[i],false);
        DEBUG_PRINT(("%d == %s",i,str))
        free(str);
    }
    DEBUG_PRINT(("** ENVIRONMENT : %d",s->environment->size)) 
    struct envnode *node = s->environment->bucket;
    while(node){
        DEBUG_PRINT(("%s at %d ",node->key,node->value))
        node = node->next;
    }
    DEBUG_PRINT(("** CONTINUATION")) 
    SValue cc = MakeSContinuation(s->continuation);
    DEBUG_PRINT((" --> cont type %s",N(tostring)(cc,false)))

    DEBUG_PRINT(("** FUNCTIONS")) 
    sfunctions * snode = s->functions;
    while(snode != NULL){ 
        DEBUG_PRINT(("--> Label == %d",snode->label))
        snode = snode->next;
    }
    DEBUG_PRINT(("==========================")) 
}

#endif


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    slhas
 *  Description:    has element ?
 * =====================================================================================
 */
FUNCTIONALITY bool slhas(sfunctions * ls,int label){
    sfunctions * node = ls;
    while(node){
        if(node->label == label){
            return true;
        }
        node = node->next;
    }
    return false;       
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    slinsert
 *  Description:    add something to environment
 * =====================================================================================
 */
FUNCTIONALITY void slinsert(sfunctions ** ls,int label){
    sfunctions * node;
    node        = malloc(sizeof(sfunctions));
    node->next  = *ls;
    node->label = label;
    *ls        = node;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    sprim_sum
 *  Description:    sum operation
 * =====================================================================================
 */
FUNCTIONALITY SValue N(prim_sum)(SValue a, SValue b) {
    return MakeSInt(a.z->value + b.z->value) ;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    prim_product
 *  Description:    product operation
 * =====================================================================================
 */
FUNCTIONALITY SValue N(prim_product)(SValue a, SValue b) {
    return MakeSInt(a.z->value * b.z->value) ;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    prim_difference
 *  Description:    difference operation
 * =====================================================================================
 */
FUNCTIONALITY SValue N(prim_difference)(SValue a, SValue b) {
    return MakeSInt(a.z->value - b.z->value) ;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    prim_numEqual
 *  Description:    equal operation
 * =====================================================================================
 */
FUNCTIONALITY SValue N(prim_numEqual)(SValue a, SValue b) {
    return MakeSBoolean(a.z->value == b.z->value) ;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSInt
 *  Description:    create a SValue INT
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSInt(int n) {
    SValue v;
    struct SInt * data = (struct SInt*) malloc(sizeof(struct SInt));
    v.z = data;
    v.z->t = SINT ;
    v.z->value = n ;
    return v ;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSBoolean
 *  Description:    create a SValue BOOLEAN
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSBoolean(unsigned int b) {
    static struct SBoolean datatrue  = {SBOOLEAN,1};
    static struct SBoolean datafalse = {SBOOLEAN,0};

    SValue v ;
    if(b){ v.b = &datatrue;}
    else{ v.b = &datafalse;}
    return v ;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeIS
 *  Description:    create a Value IS
 * =====================================================================================
 */
extern SValue MakeSI(Value n) {
    SValue v;
    struct SI * data = (struct SI*) malloc(sizeof(struct SI));
    v.i = data;
    v.i->t = SI ;
    v.i->arg = n ;
    return v ;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSIf
 *  Description:    create a SValue IF
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSIf(SValue a,SValue b,SValue c){
    SValue v;
    struct SIf * data = (struct SIf*) malloc(sizeof(struct SIf));
    v.f = data;
    v.f->t    = SIF;
    v.f->cond = a;
    v.f->cons = b;
    v.f->alt  = c;
    return v;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSLambda
 *  Description:    create a SValue LAM
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSLambda(int c,SValue body,...){

    va_list arguments;
    va_start(arguments, body); 

    SValue v;

    struct SLambda * data = (struct SLambda*) malloc(sizeof(struct SLambda));
    v.l = data;
    v.l->t = SLAM;
    v.l->nargs = c;
    v.l->body  = body;

    SValue * list = (SValue *) malloc(c*(sizeof(SValue)));
    for(int i = 0; i < c; ++i )
            list[i] = va_arg(arguments,SValue);
    
    va_end(arguments);
    v.l->arguments = list;
    
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSPrim
 *  Description:    create a SValue SPrim
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSPrim(int c,SPrimOp ex,SValue arg,...){

    va_list arguments;
    va_start(arguments, arg); 

    SValue v;
    struct SPrim * data = (struct SPrim*) malloc(sizeof(struct SPrim));
    v.p = data;
    v.p->t = SPRIM;
    v.p->exec  = ex;
    v.p->nargs = c;

    SValue * list = (SValue *) malloc(c*(sizeof(SValue)));
    list[0] = arg;
    for(int i = 1; i < c; ++i )
            list[i] = va_arg(arguments,SValue);

    va_end(arguments);
    v.p->arguments = list;

    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSApplication
 *  Description:    create a SValue APPLICATION
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSApplication(int c,SValue a,...){
    
    va_list arguments;
    va_start(arguments, a); 

    SValue v;
    struct SApplication * data = (struct SApplication*) malloc(sizeof(struct SApplication));
    v.a = data;
    v.a->t         = SAPPLICATION;
    v.a->nargs = c;
    
    SValue * list = (SValue *) malloc(c*(sizeof(SValue)));
    list[0] = a; 
    for(int i = 1; i < c; ++i )
            list[i] = va_arg(arguments,SValue);

    va_end(arguments);
    v.a->arguments = list;

    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSSymbol
 *  Description:    create a SValue SYMBOL
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSSymbol(char * name){
    SValue v;
    struct SSymbol * data = (struct SSymbol*) malloc(sizeof(struct SSymbol));
    v.s = data;
    v.s->t    = SSYMBOL;
    v.s->name = name;
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSClosure
 *  Description:    create a SClosure
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSClosure(SValue atom, N(environ) * htbl){
        SValue clos;
          
        struct SClosure * data = (struct SClosure*) malloc(sizeof(struct SClosure));
        clos.c = data;
        clos.c->t      = SCLOSURE;
        clos.c->lambda = atom;
        clos.c->env = N(copyenv)(htbl);

		return clos;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSCallcc
 *  Description:    create a SCallcc
 * =====================================================================================
 */
MAKE_(SCallcc,SCALLCC,cc,function)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSSet
 *  Description:    create a SSetV
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSSet(SValue v,SValue t){
    SValue val;

    val.sv        = (struct SSetV *) malloc(sizeof(struct SSetV));
    val.sv->t     = SSET;
    val.sv->var   = v;
    val.sv->value = t;

    return val;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSLet
 *  Description:    create a SLet
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSLet(SValue v,SValue t,SValue b){
    SValue val;

    val.lt       = (struct SLet *) malloc(sizeof(struct SLet));
    val.lt->t    = SLET;
    val.lt->var  = v;
    val.lt->expr = t;
    val.lt->body = b;

    return val;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSLetrec
 *  Description:    create a SLetrec
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSLetrec(int c,SValue v,SValue t,...){
    va_list arguments;
    va_start(arguments, t); 

    SValue val;

    val.lr        = (struct SLetrec *) malloc(sizeof(struct SLetrec));
    val.lr->t     = SLETREC;
    val.lr->nargs = c;
    val.lr->body = v;
    
    val.lr->vars = (SValue *) malloc(c*(sizeof(SValue)));
    val.lr->exprs = (SValue *) malloc(c*(sizeof(SValue)));

    val.lr->vars[0] = t; 
    for(int i = 1; i < c; ++i ){
        val.lr->vars[i] = va_arg(arguments,SValue);
    }
    for(int i = 0; i < c; ++i ){
        val.lr->exprs[i] = va_arg(arguments,SValue);
    }

    va_end(arguments);

    return val;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSContinuation
 *  Description:    internal creation of continuation
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSContinuation(skont kstar){
    SValue val;
    
    val.k = (struct SContinuation *) malloc(sizeof(struct SContinuation));
    val.k->t     = SCONTINUATION;
    val.k->kstar = kstar;
    
    return val;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSVoid
 *  Description:    return a value
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSVoid(void){
    SValue val;
    val.tt = SVOID;
    return val;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSBegin
 *  Description:    return a value
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSBegin(int c, SValue args,...){
    va_list arguments;
    va_start(arguments, args); 

    SValue v;
    struct SBegin * data = (struct SBegin*) malloc(sizeof(struct SBegin));
    v.bg = data;
    v.bg->t         = SBEGIN;
    v.bg->nargs = c;
    
    SValue * list = (SValue *) malloc(c*(sizeof(SValue)));
    list[0] = args;
    for(int i = 1; i < c; ++i )
            list[i] = va_arg(arguments,SValue);

    va_end(arguments);
    v.bg->stmts = list;

    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSCar
 *  Description:    return a value
 * =====================================================================================
 */
MAKE_(SCar,SCAR,car,arg)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSCdr
 *  Description:    return a value
 * =====================================================================================
 */
MAKE_(SCdr,SCDR,cdr,arg)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSCons
 *  Description:    return a value
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSCons(SValue v,SValue v2){
    SValue val;
    
    val.cons = (struct SCons *) malloc(sizeof(struct SCons));
    val.cons->t     = SCONS;
    val.cons->arg = v;
    val.cons->arg2 = v2;

    return val;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSList
 *  Description:    return a value
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSList(int c,SValue args,...){
    va_list arguments;
    va_start(arguments, args); 

    SValue v;
    struct SList * data = (struct SList*) malloc(sizeof(struct SList));
    v.ls = data;
    v.ls->t         = SLIST;
    v.ls->islist = 1;
    v.ls->nargs = c;
    
    SValue * list = (SValue *) malloc(c*(sizeof(SValue)));
    list[0] = args;
    for(int i = 1; i < c; ++i )
            list[i] = va_arg(arguments,SValue);

    va_end(arguments);
    v.ls->args = list;

    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeNIL
 *  Description:    return a value
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSNIL(void){
    SValue v;
    struct SList * data = (struct SList*) malloc(sizeof(struct SList));
    v.ls = data;
    v.ls->t         = SLIST;
    v.ls->nargs = 0;
    v.ls->islist = 1;
    v.ls->args = NULL;
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakePair
 *  Description:    return a value
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSPair(SValue v,SValue v2){
    SValue ret       = MakeSList(2,v,v2);
    ret.ls->islist = 0;
    return ret;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeQuote
 *  Description:    return a value
 * =====================================================================================
 */
MAKE_(SQuote,SQUOTE,q,arg)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakePairQ
 *  Description:    return a value
 * =====================================================================================
 */
MAKE_(SPairQ,SPAIRQ,pq,arg)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSListQ
 *  Description:    return a value
 * =====================================================================================
 */
MAKE_(SListQ,SLISTQ,lq,arg)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSListQ
 *  Description:    return a value
 * =====================================================================================
 */
MAKE_(SNullQ,SNULLQ,nq,arg)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSUndef
 *  Description:    return a value
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSUndef(void){
    SValue val;
    val.tt = SUNDEF;
    return val;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSDefine
 *  Description:    return a value
 * =====================================================================================
 */
FUNCTIONALITY SValue MakeSDefine(SValue v,SValue v2){
    SValue val;
    
    val.d = (struct SDefine *) malloc(sizeof(struct SDefine));
    val.d->t     = SDEFINE;
    val.d->var = v;
    val.d->expr = v2;
    return val;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    scopyvalue
 *  Description:    copy a value
 * =====================================================================================
 */
FUNCTIONALITY SValue scopyvalue(SValue par){

    switch(par.tt){

        case SVOID : 
            return MakeSVoid(); 

        case SUNDEF :
            return MakeSUndef(); 
    }

    switch(par.b->t){

    case SINT :
        return MakeSInt(par.z->value);

    case SI :
        return MakeSI(par.i->arg);
    
    case SBOOLEAN :
        return par;
    
    case SLAM :{
        SValue out;
        SValue body = scopyvalue(par.l->body);
        out.l = (struct SLambda*) malloc(sizeof(struct SLambda));
        out.l->t            = SLAM;
        out.l->body         = body;
        out.l->nargs        = par.l->nargs;
        out.l->arguments    = (SValue *) malloc((out.l->nargs)*(sizeof(SValue)));
        for(int i           = 0; i < par.l->nargs; i++){
            out.l->arguments[i] = scopyvalue(par.l->arguments[i]);
        }
        return out;
    }

    case PRIM :{
        SValue out;
        out.p            = (struct SPrim*) malloc(sizeof(struct SPrim));
        out.p->t         = SPRIM;
        out.p->exec      = par.p->exec;
        out.p->nargs     = par.p->nargs;
        out.p->arguments = (SValue *) malloc((out.p->nargs)*(sizeof(SValue)));
        for(int i = 0; i < par.p->nargs; i++){
            out.p->arguments[i] = scopyvalue(par.p->arguments[i]);
        }
        return out;
    }

    case SSYMBOL :
        return MakeSSymbol(par.s->name);

    case SAPPLICATION : COPY_(SApplication,SAPPLICATION,a,par,arguments)

    case SIF :
        return MakeSIf(scopyvalue(par.f->cond),scopyvalue(par.f->cons),scopyvalue(par.f->alt));
    
    case SCLOSURE :
        return MakeSClosure(scopyvalue(par.c->lambda),par.c->env);

    case SCONTINUATION :
        return MakeSContinuation(par.k->kstar); 

    case SCALLCC :
        return MakeSCallcc(scopyvalue(par.cc->function));

    case SSET :
        return MakeSSet(scopyvalue(par.sv->var),scopyvalue(par.sv->value)); 
    
    case SLET :
        return MakeSLet(scopyvalue(par.lt->var),scopyvalue(par.lt->expr),scopyvalue(par.lt->body));
    
    case SLETREC : {
        SValue out;
        out.lr        = (struct SLetrec *) malloc(sizeof(struct SLetrec));
        out.lr->t     = SLETREC;
        out.lr->nargs = par.lr->nargs;
        out.lr->body  = scopyvalue(par.lr->body);
        out.lr->vars  = (SValue *) malloc((out.lr->nargs)*(sizeof(SValue)));
        out.lr->exprs  = (SValue *) malloc((out.lr->nargs)*(sizeof(SValue)));
        for(int i = 0; i < out.lr->nargs; i++){
            out.lr->vars[i] = scopyvalue(par.lr->vars[i]);
        }
        for(int i = 0; i < out.lr->nargs; i++){
            out.lr->exprs[i] = scopyvalue(par.lr->exprs[i]);
        }
        return out;
    }

    case SBEGIN : COPY_(SBegin,SBEGIN,bg,par,stmts)

    case SCAR :
        return MakeSCar(scopyvalue(par.car->arg));

    case SCDR :
        return MakeSCdr(scopyvalue(par.cdr->arg));
    
    case SCONS :
        return MakeSCons(scopyvalue(par.cons->arg),scopyvalue(par.cons->arg2));

    case SLIST : {
        SValue out;
        out.ls            = (struct SList*) malloc(sizeof(struct SList));
        out.ls->t         = SLIST;
        out.ls->islist  = par.ls->islist;
        out.ls->nargs   = par.ls->nargs;
        out.ls->args = (SValue *) malloc((out.ls->nargs)*(sizeof(SValue)));
        for(int i = 0; i < par.ls->nargs; i++){
            out.ls->args[i] = scopyvalue(par.ls->args[i]);
        }
        return out;
    }


    case SQUOTE :
        return MakeSQuote(scopyvalue(par.q->arg));
    
    case SPAIRQ :
        return MakeSPairQ(scopyvalue(par.pq->arg));
    
    case SLISTQ :
        return MakeSListQ(scopyvalue(par.lq->arg));

    case SDEFINE :
        return MakeSDefine(scopyvalue(par.d->var),scopyvalue(par.d->expr));

    case SNULLQ :
        return MakeSNullQ(scopyvalue(par.nq->arg));

      default :
         DEBUG_PRINT(("Could not Copy !!!"))
         exit(1);
}}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    isatom
 *  Description:    simple check
 * =====================================================================================
 */
FUNCTIONALITY bool isatom(SValue el)
{
    enum STag atoms[] = {SLAM,SI,SINT,SSYMBOL,SBOOLEAN,SPRIM,SVOID,SLIST,SQUOTE,SCLOSURE};
    if(el.tt == SVOID||el.tt == SUNDEF) return true;
    for(int i = 0; i < NELEMS(atoms); i++){ 
        if(el.b->t == atoms[i]){return true;}
    }
    return false;
}
    

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    evalatom
 *  Description:    evaluate atomic expressions 
 *  Todo       :    implement primitives
 * =====================================================================================
 */
FUNCTIONALITY SValue evalatom(SValue atom,N(environ) * htbl,memory mem){

    switch(atom.tt){

        case SVOID :
        case SUNDEF :
            return atom;
    }

    switch(atom.b->t){

    case SSYMBOL :{
        int adress = (int) N(get)(htbl,(const char *) atom.s->name); 
        if(adress == -1){ 
            DEBUG_PRINT(("Storage failure for %s",atom.s->name))
            DEBUG_PRINT(("ENVIRONMENT : %d",htbl->size)) 
            struct N(envnode) *node = htbl->bucket;
            while(node){
                DEBUG_PRINT(("%s at %d ",node->key,node->value))
                node = node->next;
            }
            exit(1);
        }
        //sfreevalue(atom);
        SValue res = mem[adress]; 
        if(res.tt == SUNDEF) {
            DEBUG_PRINT(("Unintialized Binding to %s",atom.s->name))
            exit(1);
        }
        return res;
    }

    case SBOOLEAN :
    case SLIST:
    case SINT : 
    case SCLOSURE :
        return atom;
        break;


 /*   case SLIST : {
        SValue v;

        SValue * parsed =  (SValue *) malloc(atom.ls->nargs * sizeof(SValue));
        for(int i = 0; i < atom.ls->nargs; i++){
            parsed[i] = evalatom(atom.ls->args[i],htbl,mem);           
        }
        
        struct SList * data = malloc(sizeof(struct SList));
        data->t      = SLIST;
        data->islist = atom.ls->islist;
        data->nargs  = atom.ls->nargs;
        data->args   = parsed;
        v.ls         = data;

        return v;
    }*/

    case PRIM :{
        DEBUG_PRINT(("Starting Primitive"))
        SValue * parsed =  (SValue *) malloc(atom.p->nargs * sizeof(SValue));
        for(int i = 0; i < atom.p->nargs; i++){
            parsed[i] = evalatom(atom.p->arguments[i],htbl,mem);
        }
        SValue sum = atom.p->exec(parsed[0],parsed[1]);
        for(int i = 2; i < atom.p->nargs; i++){
            sum = atom.p->exec(sum,parsed[i]); 
        }
        free(parsed);
        //sfreevalue(atom);
        DEBUG_PRINT(("Ending Primitive"))
        return sum; 
    }

    case SI : {
        DEBUG_PRINT(("@Jumping to Insecure"))
        
        // make Continue continuation
        skont kk; 
        kk.c    = malloc(sizeof(struct scont_kont));
        kk.c->t = SKCONTINUE;
        kk.c->next = mystate->continuation;
        kk.c->e = N(copyenv)(mystate->environment);
        mystate->continuation = kk;

        Value ptr = evaluate((atom.i->arg)); 


        // No Heap
        if(ptr.tt == SVOID){
            SValue empty = {0};
            return empty;
        }

        // Descriptors
        switch(ptr.b->t){

            case LIST:{
                SValue v;

                SValue * list = malloc(ptr.ls->nargs * (sizeof(SValue))); 
                for(int i =0; i < ptr.ls->nargs; i++){
                   list[i] = MakeSI(ptr.ls->args[i]);   
                }

                struct SList * data = malloc(sizeof(struct SList));
                data->t      = SLIST;
                data->islist = ptr.ls->islist;
                data->nargs = ptr.ls->nargs;
                data->args = list;
                v.ls = data;

                return v;
            }
         
            case BOOLEAN :{
                return MakeSBoolean(ptr.b->value);
            }

            case INT :{ 
                return MakeSInt(ptr.b->value);
            }

            case CLOSURE : {
                int c = mystate->free_adr;
                mystate->storage[mystate->free_adr] = (MakeSSymbol("a"));
                slinsert(&(mystate->functions),mystate->free_adr);
                DEBUG_PRINT(("Adding Label (A) == %d",c))
                mystate->free_adr++;
                return evalatom(MakeSLambda(1,MakeSI(MakeApplication(2,ptr,MakeIS(c))),MakeSSymbol("a")),
                    mystate->environment,mystate->storage);
            }
        }

        DEBUG_PRINT(("Failed"))
        exit(1);
    }

    case SLAM :{
        SValue cpy = scopyvalue(atom); 
	    return MakeSClosure(cpy,htbl);
    }


    case SQUOTE :
        return atom.q->arg; 
        break;

    default :{ 
        DEBUG_PRINT(("Not to be used"))
        exit(1);
    } 
   }
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    applykont
 *  Description:    execute a continuation 
 * =====================================================================================
 */
FUNCTIONALITY limbo applykont(SValue val,skont k,state * s)
{
    if(k.empty == NULL){
        limbo ret = {NULL,{val,s}};
        return ret;
    }

    switch(k.l->t){

        case SKLET : { // KOONT
            struct slet_kont * lk = k.l; 
            N(insert)(lk->e,lk->var.s->name,s->free_adr);
		    s->storage[s->free_adr] = scopyvalue(val); // MEM : Don't clear  
		    s->free_adr++;
            //sfreevalue(&s->control);  
		    s->control      = lk->expr;
		    s->environment  = lk->e;
		    s->continuation = lk->next;
            limbo ret = {s,{}};
            return ret;
        }

        case SKRET :{
            s->continuation = k.r->next;     
            s->control = val;
            limbo ret = {NULL,{val,s}};
            return ret;
        }

        case SKCONTINUE : {
            s->control      = val;
            s->continuation = k.c->next;
            s->environment  = k.c->e;
            limbo ret       = {s,{}};
            return ret;
        }
        
        default :
            DEBUG_PRINT (("Unsupported Continuation"))
            exit(1);
    }
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    applyproc
 *  Description:    apply procedure to value
 * =====================================================================================
 */
FUNCTIONALITY limbo applyproc(SValue proc,SValue * args,state *s){
    DEBUG_PRINT(("CALL PROCEDURE %s",N(tostring)(proc,false)))
	if(proc.c->t == SCLOSURE){

        int curr = s->free_adr;
        int nargs = proc.c->lambda.l->nargs; 
        DEBUG_PRINT(("Proc arg == %d",nargs))
        s->free_adr = curr + nargs; 

		// update enviroment with new adresses for each variable of lambda
        for(int j = curr,i = 0; j < s->free_adr ;j++){
            N(insert)(proc.c->env,proc.c->lambda.l->arguments[i].s->name,j); 
            i++;
        }

		// update storage with adresses pointing to arguments
        for(int j = curr,i = 0; j < s->free_adr ;j++){
            DEBUG_PRINT(("Proc %d == %s",i,N(tostring)((args[i]),false)))
            s->storage[j] = scopyvalue(args[i]); // MEM : Don't clear
            i++;
        }
        
		// create new state with updated storage and envorinment, control = body of lambda
        s->environment = N(copyenv)(proc.c->env);
        SValue cpy = scopyvalue(proc.c->lambda.l->body);
        //sfreevalue(&s->control);
        s->control = cpy; 
        limbo ret = {s,{}};
        return ret;
	}
	else if(proc.k->t == SCONTINUATION){
        DEBUG_PRINT((">>>>> DOING CONTINUATION"))
		return applykont(args[0],proc.k->kstar, s);
	}
	else{
		DEBUG_PRINT(("Unkown Procedure"))
        exit(1);
	}
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    step
 *  Description:    execute a step in the CESK machine 
 *  Todo       :	Done ? 
 * =====================================================================================
 */
FUNCTIONALITY limbo step(state * s)
{
    // atom case
    if(isatom(s->control))
    {
        SValue return_val = evalatom(s->control,s->environment,s->storage);
        return  applykont(return_val,s->continuation,s); 
    }

    switch(s->control.b->t){

    case SIF :{
        SValue condi = evalatom(s->control.f->cond,s->environment,s->storage);	

		if(condi.b->value == 1){
            SValue cpy = scopyvalue(s->control.f->cons);
            //sfreevalue(&s->control);
			s->control = cpy;
		}else{
            SValue cpy = scopyvalue(s->control.f->alt);
            //sfreevalue(&s->control);
			s->control = cpy;	
		}

        limbo ret = {s,{}};
		return ret;
	}

    case SAPPLICATION :{
        DEBUG_PRINT(("Appl arg == %d",s->control.a->nargs))
        SValue * argum = (SValue *) malloc(sizeof(SValue) * s->control.a->nargs);
        for(int i = 0; i < s->control.a->nargs; i++){
            argum[i] = evalatom(s->control.a->arguments[i],s->environment,s->storage);
        }
        limbo res = applyproc(argum[0],(++argum),s);
        free(--argum);
        return res;
    }

    case SCALLCC :{
        SValue proc = evalatom(s->control.cc->function,s->environment,s->storage);
        SValue curr = MakeSContinuation(s->continuation);
        limbo res = applyproc(proc,&curr,s);
        return res;
    }

    case SSET : {
        SValue val = evalatom(s->control.sv->value,s->environment,s->storage);
        int adress = (int) N(get)(s->environment,(const char *) s->control.sv->var.s->name); 
        FREECELL((s->storage[adress]))
        s->storage[adress] = scopyvalue(val); // MEM 
        SValue empty = {0};
        return applykont(empty,s->continuation,s);
    }

    case SDEFINE : {
        if( s->control.d->var.s->t != SYMBOL) {DEBUG_PRINT(("Expected Symbol !!")) exit(1);}
        int test = (int) N(get)(s->environment,(const char *) s->control.d->var.s->name); 
        if(test == -1){ 
            N(insert)(s->environment,(const char *)s->control.d->var.s->name,s->free_adr++);
        }        
        // once binding exists preform set
        SValue val = evalatom(s->control.d->expr,s->environment,s->storage);
        int adress = (int) N(get)(s->environment,(const char *) s->control.d->var.s->name); 
        FREECELL((s->storage[adress]))
        s->storage[adress] = scopyvalue(val); // MEM
        SValue empty = {0};
        return applykont(empty,s->continuation,s);
    }

    case SLET : {
        SValue a = scopyvalue(s->control.lt->expr);
        SValue b = scopyvalue(s->control.lt->var);
        SValue c = scopyvalue(s->control.lt->body);
        //sfreevalue(&(s->control));
        s->control = a;

        // Make a letk with b,c and environment and previous cont
        struct slet_kont * nn = malloc(sizeof(struct slet_kont));
        nn->var  = b;
        nn->expr = c;
        nn->e    = s->environment; 
        nn->next = s->continuation;
        nn->t = KLET;
        skont new;
        new.l = nn;
        s->continuation = new;
        limbo ret = {s,{}};
		return ret;
    }

    case SLETREC : {
        int curr = s->free_adr;
        int nargs = s->control.lr->nargs; 
        s->free_adr = curr + nargs; 
		// update enviroment with new adresses for each variable of lambda
        for(int j = curr,i = 0; j < s->free_adr ;j++){
            N(insert)(s->environment,(const char *)s->control.lr->vars[i].s->name,j); 
            i++;
        }
        SValue * list = (SValue*) malloc(s->control.lr->nargs * (sizeof(SValue)));
        for(int i = 0; i < s->control.lr->nargs ;i++){
            // curry ?
            list[i] = evalatom(s->control.lr->exprs[i],s->environment,s->storage);
        }
		// update storage with adresses pointing to arguments
        for(int j = curr,i = 0; j < s->free_adr ;j++){
            s->storage[j] = scopyvalue(list[i]); // MEM : new adress do not delete
            i++;
        }
        SValue b = scopyvalue(s->control.lr->body);
        //sfreevalue(&(s->control));
        s->control = b;
        limbo ret = {s,{}};
		return ret;
    } 

    case SBEGIN : {
        SValue proc;
        for(int i = 0; i < s->control.bg->nargs; i++){ 
            proc = evalatom(s->control.bg->stmts[i],
                s->environment,s->storage);
        }
        return  applykont(proc,s->continuation,s); 
    }

    case SCAR : {
        struct SCar * here = s->control.car;
        SValue val = evalatom(here->arg,s->environment,s->storage);
        if(val.ls->t == LIST){
            if (val.ls->nargs > 0){
                return applykont(val.ls->args[0],s->continuation,s); 
            }else {
                DEBUG_PRINT(("Empty List!!"))
                exit(1);
            }
        }
        else{
            DEBUG_PRINT(("Expected List"))
            exit(1);
        }
    }

    case SCDR : {
        struct SCdr * here = s->control.cdr;
        SValue val = evalatom(here->arg,s->environment,s->storage);
        if(val.ls->t == LIST){
            if (val.ls->nargs > 1){
                SValue * newlist = (SValue *) malloc((val.ls->nargs-1) * (sizeof (SValue))); 
                for(int i = 1; i < val.ls->nargs; i++){
                    newlist[(i-1)] = scopyvalue(val.ls->args[i]); 
                }
                free(val.ls->args);
                val.ls->args = newlist;
                val.ls->nargs--;
                return applykont(val,s->continuation,s); 
            }else if(val.ls->nargs == 1) {
                return applykont(MakeSNIL(),s->continuation,s);
            }
            else{
                DEBUG_PRINT(("Empty List!!"))
                exit(1);
            }
        }
        else{
            DEBUG_PRINT(("Expected List"))
            exit(1);
        }
    }
    case SCONS : {
        SValue v = evalatom(s->control.cons->arg,s->environment,s->storage);
        SValue v2 = evalatom(s->control.cons->arg2,s->environment,s->storage);
        if(v2.ls->t == LIST && v2.ls->islist == true){
            SValue * newlist = (SValue *) malloc((v2.ls->nargs+1) * (sizeof (SValue)));  
            newlist[0] = v;
            for(int i = 0; i < v2.ls->nargs; i++){
                newlist[(i+1)] = scopyvalue(v2.ls->args[i]); 
            }
            free(v2.ls->args);
            v2.ls->args = newlist;
            v2.ls->nargs++;
            return applykont(v2,s->continuation,s);   
        }
        return applykont(MakeSPair(v,v2),s->continuation,s);   
    }

    case SPAIRQ : {
        SValue v = evalatom(s->control.pq->arg,s->environment,s->storage);  
        if(v.ls->t == LIST){
            if(v.ls->islist == true){
                return applykont(MakeSBoolean((v.ls->nargs > 0)),s->continuation,s);  
            }
            return applykont(MakeSBoolean((v.ls->nargs > 1)),s->continuation,s);  
        }
        else{
            DEBUG_PRINT(("Expected List"))
            exit(1);
        }
    }

    case SLISTQ : {
        SValue v = evalatom(s->control.lq->arg,s->environment,s->storage);  
        if(v.ls->t == LIST){
            return applykont(MakeSBoolean(v.ls->islist),s->continuation,s); 
        }
        else{
            DEBUG_PRINT(("Expected List"))
            exit(1);
        }
    }   
    
    case SNULLQ : {
        SValue v = evalatom(s->control.nq->arg,s->environment,s->storage);  
        if(v.ls->t == LIST){
            return applykont(MakeSBoolean(v.ls->nargs == 0),s->continuation,s); 
        }
        else{
            DEBUG_PRINT(("Expected List"))
            exit(1);
        }
    }   

    default :
		DEBUG_PRINT(("Unkown State"))
        exit(1);
    break;

}}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    steprec
 *  Description:    step through program
 * =====================================================================================
 */
FUNCTIONALITY answer steprec (state * s){
    static int i = 0;
    DEBUG_PRINT(("STEP# == %d of Secure",++i))
    int log = i;
    #ifdef DEBUG
        debugstate(s);
    #endif
    limbo result = step(s); 
    if(result.computation == NULL){
        DEBUG_PRINT(("DONE Secure STEP#== %d",log))
        return result.ans;
    }
    else{
       return steprec(result.computation);
    }
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    inject
 *  Description:    create a new start state
 * =====================================================================================
 */
FUNCTIONALITY void inject (){

    static N(environ) tbl = {NULL,0};

    if(mystate != NULL) { free(mystate); }

    // create empty environment -- tested
    N(environ) * envtable = MALLOC (sizeof(N(environ)));  
    *envtable = tbl;

    // inject state
    mystate = malloc(sizeof(state));
    mystate->environment = envtable;
    mystate->storage = (SValue *) (malloc(MEM_SIZE * sizeof(SValue))); 
    mystate->continuation.empty = NULL;
    mystate->functions = NULL;
	mystate->free_adr = 0;
}



/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    secure_eval
 *  Description:    the entry point for the outside
 * =====================================================================================
 */
ENTRYPOINT void * secure_eval(int label){

    SValue sec;
    DEBUG_PRINT(("Check :: label == %d",label))
    if(slhas(mystate->functions,label)){
        mystate->control = mystate->storage[label];
        DEBUG_PRINT(("Check Succeeded"))

        // Make Return Continuation
        skont kk;
        kk.r                  = malloc(sizeof(struct sret_kont));
        kk.r->t               = SKRET;
        kk.r->next            = mystate->continuation;
        mystate->continuation = kk;
        
        // compute
        answer ans = steprec(mystate);
        SValue in = ans.ans; // input

        // No Heap
        if(in.tt == SVOID){
            Value empty = {0};
            return empty.b;
        }
    
        // Descriptors
        switch(in.b->t){

            case SLIST:{
                Value v;

                Value * list = malloc(in.ls->nargs * (sizeof(Value))); 
                for(int i =0; i < in.ls->nargs; i++){
                    int d = mystate->free_adr; 
                    mystate->storage[mystate->free_adr] = in.ls->args[i];
                    slinsert(&(mystate->functions),mystate->free_adr);
                    DEBUG_PRINT(("Adding Label (A) == %d",d))
                    mystate->free_adr++;
                    list[i] = MakeIS(d);   
                }

                struct List * data = malloc(sizeof(struct List));
                data->t      = LIST;
                data->islist = in.ls->islist;
                data->nargs = in.ls->nargs;
                data->args = list;
                v.ls = data;

                return v.b;
            }

            case SBOOLEAN :{
                return (MakeBoolean(ans.ans.b->value)).b; 
            }

            case SINT:{
                return (MakeInt(ans.ans.z->value)).b; 
            }

            case SCLOSURE : {
                int c = mystate->free_adr;
                mystate->free_adr++;
                slinsert(&(mystate->functions),c);
                mystate->storage[c] = (MakeSApplication(2,ans.ans,MakeSI(MakeSymbol("z"))));
                DEBUG_PRINT(("Return :: Adding Label == %d",c))
                return (MakeLambda(1,MakeIS(c),MakeSymbol("z"))).b; 
            }
        }

        DEBUG_PRINT(("Invalid Return Value"))
        exit(1); 
    }

    return -1;
}

/*
REPLACE */


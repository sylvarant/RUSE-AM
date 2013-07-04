/*
 * =====================================================================================
 *
 *       Filename:  scheme.c
 *
 *    Description:  Scheme Language descriptors implementation
 *
 *        Created:  07/01/2013 19:30:53
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#include "cesk.h" 
#include <stdarg.h> // TODO lose dependency ?


/*-----------------------------------------------------------------------------
 *  Preprocessor
 *-----------------------------------------------------------------------------*/

// Compress similar make definitions
#define MAKE_(TYPE,TAG,ID,ARG) FUNCTIONALITY VALUE N(make##TYPE)(VALUE v){\
    VALUE val;\
    val.ID      = MALLOC(sizeof(struct TYPE));\
    val.ID->t       = N(TAG);\
    val.ID->ARG = v; \
    return val;\
}

// Compress similar copy definitions
#define COPY_(TYPE,TAG,ID,ARG,AA){\
    VALUE out;\
    out.ID = MALLOC (sizeof(struct N(TYPE)));\
    out.ID->t     = N(TAG);\
    out.ID->nargs = ARG.ID->nargs;\
    out.ID->AA = MALLOC( ARG.ID->nargs * (sizeof (VALUE)));\
    for(int i = 0; i < ARG.ID->nargs; i++){\
            out.ID->AA[i] = N(copyValue)(ARG.ID->AA[i]);}\
    return out;}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    prim_sum
 *  Description:    sum operation
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(sumPrim)(VALUE a, VALUE b) {
    return N(makeInt)(a.z->value + b.z->value) ;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    prim_product
 *  Description:    product operation
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(productPrim)(VALUE a, VALUE b) {
    return N(makeInt)(a.z->value * b.z->value) ;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    prim_difference
 *  Description:    difference operation
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(differencePrim)(VALUE a, VALUE b) {
    return N(makeInt)(a.z->value - b.z->value) ;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    prim_numEqual
 *  Description:    equal operation
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(numequalPrim)(VALUE a, VALUE b) {
    return N(makeBoolean)(a.z->value == b.z->value) ;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeInt
 *  Description:    create a VALUE INT
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makeInt)(int n) {
    VALUE v;
    v.z = MALLOC(sizeof(struct N(Int)));
    v.z->t = N(INT) ;
    v.z->value = n ;
    return v ;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeBoolean
 *  Description:    create a VALUE BOOLEAN
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makeBoolean)(unsigned int b) {

    // TODO this optimization should be more visible
    static struct N(Boolean) datatrue  = {N(BOOLEAN),1};
    static struct N(Boolean) datafalse = {N(BOOLEAN),0};

    VALUE v ;
    if(b){ v.b = &datatrue;}
    else{ v.b = &datafalse;}
    return v ;
}

#ifdef SECURE

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeSI
 *  Description:    create a VALUE across the boundary to the InSecure
 * =====================================================================================
 */
FUNCTIONALITY VALUE makeSI(OTHERVALUE n) {
    VALUE v;
    v.i = MALLOC(sizeof(struct SI));
    v.i->t = SI ;
    v.i->arg = n ;
    return v ;
}

#else

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeIS
 *  Description:    create a Value across the boundary to the Secure
 * =====================================================================================
 */
FUNCTIONALITY Value makeIS(int n) {
    Value v;
    struct IS * data = (struct IS*) malloc(sizeof(struct IS));
    v.i = data;
    v.i->t = IS ;
    v.i->label = n ;
    return v ;
}
#endif

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeIf
 *  Description:    create a If VALUE : if a then b else c
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makeIf)(VALUE a,VALUE b,VALUE c){
    VALUE v;
    v.f =  MALLOC(sizeof(struct N(If)));
    v.f->t    = N(IF);
    v.f->cond = a;
    v.f->cons = b;
    v.f->alt  = c;
    return v;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeLambda
 *  Description:    create a λ VALUE with c variables 
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makeLambda)(int c,VALUE body,...){

    va_list arguments;
    va_start(arguments, body); 
    VALUE v;

    v.l = MALLOC(sizeof(struct N(Lambda)));
    v.l->t = N(LAM);
    v.l->nargs = c;
    v.l->body  = body;
    VALUE * list =  MALLOC(c * (sizeof(VALUE)));

    for(int i = 0; i < c; ++i )
            list[i] = va_arg(arguments,VALUE);
    
    va_end(arguments);
    v.l->arguments = list;
    return v;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makePrim
 *  Description:    create a Primitive computation VALUE: (ex i_1 ... i_c)
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makePrim)(int c,N(PrimOp) ex,VALUE arg,...){

    va_list arguments;
    va_start(arguments, arg); 

    VALUE v;
    v.p = MALLOC(sizeof(struct N(Prim)));
    v.p->t = N(PRIM);
    v.p->exec  = ex;
    v.p->nargs = c;
    VALUE * list = MALLOC(c*(sizeof(VALUE)));
    list[0] = arg;

    for(int i = 1; i < c; ++i )
            list[i] = va_arg(arguments,VALUE);

    va_end(arguments);
    v.p->arguments = list;
    return v;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeApplication
 *  Description:    create a Application VALUE with c arguments
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makeApplication)(int c,VALUE a,...){
    
    va_list arguments;
    va_start(arguments, a); 

    VALUE v;
    v.a = MALLOC(sizeof(struct N(Application)));
    v.a->t         = N(APPLICATION);
    v.a->nargs = c;
    
    VALUE * list = MALLOC(c*(sizeof(VALUE)));
    list[0] = a; 
    for(int i = 1; i < c; ++i )
            list[i] = va_arg(arguments,VALUE);

    va_end(arguments);
    v.a->arguments = list;
    return v;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeSymbol
 *  Description:    create a SYMBOL VALUE  with given name
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makeSymbol)(char * name){
    VALUE v;
    v.s = MALLOC(sizeof(struct N(Symbol)));
    v.s->t    = N(SYMBOL);
    v.s->name = name;
    return v;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeClosure
 *  Description:    create a Closure VALUE for a given lambda and environment
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makeClosure)(VALUE lambda, BINDING * env){
        VALUE clos;
        clos.c = MALLOC(sizeof(struct N(Closure)));
        clos.c->t      = N(CLOSURE);
        clos.c->lambda = lambda;
        clos.c->env = N(copyBinding)(env);
		return clos;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeCallcc
 *  Description:    create a Callcc VALUE
 * =====================================================================================
 */
MAKE_(Callcc,CALLCC,cc,function)


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeSet
 *  Description:    create a SetV
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makeSet)(VALUE v,VALUE t){
    VALUE val;
    val.sv        = MALLOC(sizeof(struct N(SetV)));
    val.sv->t     = N(SET);
    val.sv->var   = v;
    val.sv->value = t;
    return val;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeLet
 *  Description:    create a Let VALUE
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makeLet)(VALUE v,VALUE t,VALUE b){
    VALUE val;
    val.lt       = MALLOC(sizeof(struct N(Let)));
    val.lt->t    = N(LET);
    val.lt->var  = v;
    val.lt->expr = t;
    val.lt->body = b;
    return val;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeLetrec
 *  Description:    create a Letrec VALUE
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makeLetrec)(int c,VALUE v,VALUE t,...){
    va_list arguments;
    va_start(arguments, t); 
    VALUE val;

    val.lr        = MALLOC(sizeof(struct N(Letrec)));
    val.lr->t     = N(LETREC);
    val.lr->nargs = c;
    val.lr->body  = v;
    val.lr->vars  = MALLOC(c * (sizeof(VALUE)));
    val.lr->exprs = MALLOC(c * (sizeof(VALUE)));
    val.lr->vars[0] = t; 

    for(int i = 1; i < c; ++i ){
        val.lr->vars[i] = va_arg(arguments,VALUE);
    }

    for(int i = 0; i < c; ++i ){
        val.lr->exprs[i] = va_arg(arguments,VALUE);
    }

    va_end(arguments);
    return val;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeContinuation
 *  Description:    creation of continuation VALUE, which is simply a wrapper 
 *                  around N(Kont)
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makeContinuation)(KONT kstar){
    VALUE val;
    val.k        = MALLOC(sizeof(struct N(Continuation)));
    val.k->t     = N(CONTINUATION);
    val.k->kstar = kstar;
    return val;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeVoid
 *  Description:    create a simple void statement
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makeVoid)(void){
    VALUE val;
    val.tt = N(VOID);
    return val;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeNop
 *  Description:    create a simple no operation statement
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makeNop)(void){
    VALUE val;
    val.tt = N(NOP);
    return val;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeBegin
 *  Description:    create begin with c sub expressions
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makeBegin)(int c, VALUE args,...){
    va_list arguments;
    va_start(arguments, args); 
    VALUE v;

    v.bg = MALLOC(sizeof(struct N(Begin)));
    v.bg->t         = N(BEGIN);
    v.bg->nargs = c;
    VALUE * list = MALLOC(c*(sizeof(VALUE)));
    list[0] = args;

    for(int i = 1; i < c; ++i )
            list[i] = va_arg(arguments,VALUE);

    va_end(arguments);
    v.bg->stmts = list;
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeCar
 *  Description:    create a car VALUE to which returns the first element of a list
 * =====================================================================================
 */
MAKE_(Car,CAR,car,arg)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeCdr
 *  Description:    create a cdr VALUE which returns the last element of a list
 * =====================================================================================
 */
MAKE_(Cdr,CDR,cdr,arg)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeCons
 *  Description:    create a cons VALUE which adds two lists to produce a new one
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makeCons)(VALUE v,VALUE v2){
    VALUE val;
    val.cons       = MALLOC(sizeof(struct N(Cons)));
    val.cons->t    = N(CONS);
    val.cons->arg  = v;
    val.cons->arg2 = v2;
    return val;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeList
 *  Description:    create a list of c elements
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makeList)(int c,VALUE args,...){
    va_list arguments;
    va_start(arguments, args); 
    VALUE v;

    v.ls         = MALLOC(sizeof(struct N(List)));
    v.ls->t      = N(LIST);
    v.ls->islist = 1;
    v.ls->nargs  = c;
    VALUE * list = MALLOC(c*(sizeof(VALUE)));
    list[0]      = args;

    for(int i = 1; i < c; ++i )
            list[i] = va_arg(arguments,VALUE);

    va_end(arguments);
    v.ls->args = list;
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeNIL
 *  Description:    create an empty list
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makeNIL)(void){
    VALUE v;
    v.ls         = MALLOC(sizeof(struct N(List)));
    v.ls->t      = N(LIST);
    v.ls->nargs  = 0;
    v.ls->islist = 1;
    v.ls->args   = NULL;
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makePair
 *  Description:    return a pair (v,v2)
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makePair)(VALUE v,VALUE v2){
    VALUE ret      = N(makeList)(2,v,v2);
    ret.ls->islist = 0;
    return ret;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeQuote
 *  Description:    create 'arg
 * =====================================================================================
 */
MAKE_(Quote,QUOTE,q,arg)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makePairQ
 *  Description:    create a pair? statement
 * =====================================================================================
 */
MAKE_(PairQ,PAIRQ,pq,arg)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeListQ
 *  Description:    create a list? statement
 * =====================================================================================
 */
MAKE_(ListQ,LISTQ,lq,arg)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeNullQ
 *  Description:    create a null? statement 
 * =====================================================================================
 */
MAKE_(NullQ,NULLQ,nq,arg)

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeDefine
 *  Description:    create a Define statement
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(makeDefine)(VALUE v,VALUE v2){
    VALUE val;
    val.d       = MALLOC(sizeof(struct N(Define)));
    val.d->t    = N(DEFINE);
    val.d->var  = v;
    val.d->expr = v2;
    return val;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    copyValue
 *  Description:    copy a VALUE's memory contents
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(copyValue)(VALUE par){

    // No heap involved
    switch(par.tt){

        case N(NOP) :
            return N(makeNop());
        case N(VOID) : 
            return N(makeVoid)(); 

        default : break;
    }

    switch(par.b->t){

        case N(INT) :
            return N(makeInt)(par.z->value);

        #ifdef SECURE
        case SI :
            return makeSI(par.i->arg);
        #endif
    
        case N(BOOLEAN) :
            return par;
    
        case N(LAM) :{
            VALUE out;
            VALUE body       = N(copyValue)(par.l->body);
            out.l            =  MALLOC(sizeof(struct N(Lambda)));
            out.l->t         = N(LAM);
            out.l->body      = body;
            out.l->nargs     = par.l->nargs;
            out.l->arguments = MALLOC((out.l->nargs)*(sizeof(VALUE)));

            for(int i = 0; i < par.l->nargs; i++){
                out.l->arguments[i] = N(copyValue)(par.l->arguments[i]);
            }

            return out;
        }

        case N(PRIM) :{
            VALUE out;
            out.p            = MALLOC(sizeof(struct N(Prim)));
            out.p->t         = N(PRIM);
            out.p->exec      = par.p->exec;
            out.p->nargs     = par.p->nargs;
            out.p->arguments = MALLOC((out.p->nargs)*(sizeof(VALUE)));

            for(int i = 0; i < par.p->nargs; i++){
                out.p->arguments[i] = N(copyValue)(par.p->arguments[i]);
            }

            return out;
        }

        case N(SYMBOL) :
            return N(makeSymbol)(par.s->name);

        case N(APPLICATION) : COPY_(Application,APPLICATION,a,par,arguments)

        case N(IF) :
            return N(makeIf)(N(copyValue)(par.f->cond),N(copyValue)(par.f->cons),N(copyValue)(par.f->alt));
    
        case N(CLOSURE) :
            return N(makeClosure)(N(copyValue)(par.c->lambda),par.c->env);

        case N(CONTINUATION) :
            return N(makeContinuation)(par.k->kstar); 

        case N(CALLCC) :
            return N(makeCallcc)(N(copyValue)(par.cc->function));

        case N(SET) :
            return N(makeSet)(N(copyValue)(par.sv->var),N(copyValue)(par.sv->value)); 
    
        case N(LET) :
            return N(makeLet)(N(copyValue)(par.lt->var),N(copyValue)(par.lt->expr),N(copyValue)(par.lt->body));
    
        case N(LETREC) : {
            VALUE out;
            out.lr         = MALLOC(sizeof(struct N(Letrec)));
            out.lr->t      = N(LETREC);
            out.lr->nargs  = par.lr->nargs;
            out.lr->body   = N(copyValue)(par.lr->body);
            out.lr->vars   = MALLOC((out.lr->nargs)*(sizeof(VALUE)));
            out.lr->exprs  = MALLOC((out.lr->nargs)*(sizeof(VALUE)));

            for(int i = 0; i < out.lr->nargs; i++){
                out.lr->vars[i] = N(copyValue)(par.lr->vars[i]);
            }

            for(int i = 0; i < out.lr->nargs; i++){
                out.lr->exprs[i] = N(copyValue)(par.lr->exprs[i]);
            }

            return out;
        }

        case N(BEGIN) : COPY_(Begin,BEGIN,bg,par,stmts)

        case N(CAR) : 
            return N(makeCar)(N(copyValue)(par.car->arg));

        case N(CDR) :
            return N(makeCdr)(N(copyValue)(par.cdr->arg));
    
        case N(CONS) :
            return N(makeCons)(N(copyValue)(par.cons->arg),N(copyValue)(par.cons->arg2));

        case N(LIST) : {
            VALUE out;
            out.ls         = MALLOC(sizeof(struct N(List)));
            out.ls->t      = N(LIST);
            out.ls->islist = par.ls->islist;
            out.ls->nargs  = par.ls->nargs;
            out.ls->args   = MALLOC((out.ls->nargs)*(sizeof(VALUE)));

            for(int i = 0; i < par.ls->nargs; i++){
                out.ls->args[i] = N(copyValue)(par.ls->args[i]);
            }
            return out;
        }

        case N(QUOTE) :
            return N(makeQuote)(N(copyValue)(par.q->arg));
    
        case N(PAIRQ) :
            return N(makePairQ)(N(copyValue)(par.pq->arg));
    
        case N(LISTQ) :
            return N(makeListQ)(N(copyValue)(par.lq->arg));

        case N(DEFINE) :
            return N(makeDefine)(N(copyValue)(par.d->var),N(copyValue)(par.d->expr));

        case N(NULLQ) :
        return N(makeNullQ)(N(copyValue)(par.nq->arg));

        default :
            DEBUG_PRINT(("Could not Copy !!!"))
            exit(1);
}}



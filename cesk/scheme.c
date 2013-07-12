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


/*-----------------------------------------------------------------------------
 *  Preprocessor
 *-----------------------------------------------------------------------------*/

// Compress similar make definitions
#define MAKE_(TYPE,TAG,ID,ARG) FUNCTIONALITY void* N(make##TYPE)(void* v){\
    VALUE val;\
    val.ID      = MALLOC(sizeof(struct TYPE));\
    val.ID->t       = N(TAG);\
    val.ID->ARG.b = v; \
    return val.b;\
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
FUNCTIONALITY void* N(sumPrim)(void* a,void* b) {
    VALUE aa;VALUE bb;
    aa.b = a;
    bb.b = b;
    return N(makeInt)(aa.z->value + bb.z->value);
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    prim_product
 *  Description:    product operation
 * =====================================================================================
 */
FUNCTIONALITY void* N(productPrim)(void* a, void* b) {
    VALUE aa;VALUE bb;
    aa.b = a;
    bb.b = b;
    return N(makeInt)(aa.z->value * bb.z->value);
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    prim_difference
 *  Description:    difference operation
 * =====================================================================================
 */
FUNCTIONALITY void* N(differencePrim)(void* a, void* b) {
    VALUE aa;VALUE bb;
    aa.b = a;
    bb.b = b;
    return N(makeInt)(aa.z->value - bb.z->value);
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    prim_numEqual
 *  Description:    equal operation
 * =====================================================================================
 */
FUNCTIONALITY void* N(numequalPrim)(void* a, void* b) {
    VALUE aa;VALUE bb;
    aa.b = a;
    bb.b = b;
    return N(makeBoolean)(aa.z->value == bb.z->value);
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeInt
 *  Description:    create a VALUE INT
 * =====================================================================================
 */
FUNCTIONALITY void* N(makeInt)(int n) {
    VALUE v;
    v.z = MALLOC(sizeof(struct N(Int)));
    v.z->t = N(INT) ;
    v.z->value = n ;
    return v.b;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeBoolean
 *  Description:    create a VALUE BOOLEAN
 * =====================================================================================
 */
FUNCTIONALITY void* N(makeBoolean)(unsigned int b) {

    // TODO this optimization should be more visible
    static struct N(Boolean) datatrue  = {N(BOOLEAN),1};
    static struct N(Boolean) datafalse = {N(BOOLEAN),0};

    VALUE v ;
    if(b){ v.b = &datatrue;}
    else{ v.b = &datafalse;}
    return v.b;
}

#ifdef SECURE

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeSI
 *  Description:    create a VALUE across the boundary to the InSecure
 * =====================================================================================
 */
FUNCTIONALITY void* makeSI(void* n) {
    VALUE v;
    v.i = MALLOC(sizeof(struct SI));
    v.i->t = SI ;
    v.i->arg.b = n ;
    return v.b;
}

#else

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeIS
 *  Description:    create a Value across the boundary to the Secure
 * =====================================================================================
 */
FUNCTIONALITY void* makeIS(int n) {
    Value v;
    struct IS * data = (struct IS*) malloc(sizeof(struct IS));
    v.i = data;
    v.i->t = IS ;
    v.i->label = n ;
    return v.b;
}
#endif

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeIf
 *  Description:    create a If VALUE : if a then b else c
 * =====================================================================================
 */
FUNCTIONALITY void* N(makeIf)(void* a,void* b,void* c){
    VALUE v;
    v.f =  MALLOC(sizeof(struct N(If)));
    v.f->t    = N(IF);
    v.f->cond.b = a;
    v.f->cons.b = b;
    v.f->alt.b  = c;
    return v.b;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeLambda
 *  Description:    create a λ VALUE with c variables 
 * =====================================================================================
 */
FUNCTIONALITY void* N(makeLambda)(int c,void* body,VALUE *ls){

    VALUE v;
    v.l = MALLOC(sizeof(struct N(Lambda)));
    v.l->t = N(LAM);
    v.l->nargs = c;
    v.l->body.b  = body;
    v.l->arguments = ls;
    return v.b;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makePrim
 *  Description:    create a Primitive computation VALUE: (ex i_1 ... i_c)
 * =====================================================================================
 */
FUNCTIONALITY void* N(makePrim)(int c,N(PrimOp) ex,VALUE *ls){
    VALUE v;
    v.p = MALLOC(sizeof(struct N(Prim)));
    v.p->t = N(PRIM);
    v.p->exec  = ex;
    v.p->nargs = c;
    v.p->arguments = ls;
    return v.b;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeApplication
 *  Description:    create a Application VALUE with c arguments
 * =====================================================================================
 */
FUNCTIONALITY void* N(makeApplication)(int c,VALUE * list){
    VALUE v;
    v.a = MALLOC(sizeof(struct N(Application)));
    v.a->t         = N(APPLICATION);
    v.a->nargs = c;
    v.a->arguments = list;
    return v.b;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeSymbol
 *  Description:    create a SYMBOL VALUE  with given name
 * =====================================================================================
 */
FUNCTIONALITY void* N(makeSymbol)(char * name){
    VALUE v;
    v.s = MALLOC(sizeof(struct N(Symbol)));
    v.s->t    = N(SYMBOL);
    v.s->name = name;
    return v.b;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeClosure
 *  Description:    create a Closure VALUE for a given lambda and environment
 * =====================================================================================
 */
FUNCTIONALITY void* N(makeClosure)(void* lambda, BINDING * env){
        VALUE clos;
        clos.c = MALLOC(sizeof(struct N(Closure)));
        clos.c->t      = N(CLOSURE);
        clos.c->lambda.b = lambda;
        clos.c->env = env;
		return clos.b;
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
FUNCTIONALITY void* N(makeSet)(void* v,void* t){
    VALUE val;
    val.sv        = MALLOC(sizeof(struct N(SetV)));
    val.sv->t     = N(SET);
    val.sv->var.b   = v;
    val.sv->value.b = t;
    return val.b;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeLet
 *  Description:    create a Let VALUE
 * =====================================================================================
 */
FUNCTIONALITY void* N(makeLet)(void* v,void* t,void* b){
    VALUE val;
    val.lt       = MALLOC(sizeof(struct N(Let)));
    val.lt->t    = N(LET);
    val.lt->var.b  = v;
    val.lt->expr.b = t;
    val.lt->body.b = b;
    return val.b;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeLetrec
 *  Description:    create a Letrec VALUE
 * =====================================================================================
 */
FUNCTIONALITY void* N(makeLetrec)(int c,void* v,VALUE * t){
    VALUE val;
    val.lr        = MALLOC(sizeof(struct N(Letrec)));
    val.lr->t     = N(LETREC);
    val.lr->nargs = c;
    val.lr->body.b  = v;
    val.lr->vars  = t;
    val.lr->exprs = (t+c);
    return val.b;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeContinuation
 *  Description:    creation of continuation VALUE, which is simply a wrapper 
 *                  around N(Kont)
 * =====================================================================================
 */
FUNCTIONALITY void* N(makeContinuation)(KONT kstar){
    VALUE val;
    val.k        = MALLOC(sizeof(struct N(Continuation)));
    val.k->t     = N(CONTINUATION);
    val.k->kstar = kstar;
    return val.b;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeVoid
 *  Description:    create a simple void statement
 * =====================================================================================
 */
FUNCTIONALITY void* N(makeVoid)(void){
    VALUE val;
    val.tt = N(VOID);
    return val.b;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeNop
 *  Description:    create a simple no operation statement
 * =====================================================================================
 */
FUNCTIONALITY void* N(makeNop)(void){
    VALUE val;
    val.tt = N(NOP);
    return val.b;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeBegin
 *  Description:    create begin with c sub expressions
 * =====================================================================================
 */
FUNCTIONALITY void* N(makeBegin)(int c, VALUE * args){

    VALUE v;
    v.bg = MALLOC(sizeof(struct N(Begin)));
    v.bg->t         = N(BEGIN);
    v.bg->nargs = c;
    v.bg->stmts = args;
    return v.b;
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
FUNCTIONALITY void* N(makeCons)(void* v,void* v2){
    VALUE val;
    val.cons       = MALLOC(sizeof(struct N(Cons)));
    val.cons->t    = N(CONS);
    val.cons->arg.b  = v;
    val.cons->arg2.b = v2;
    return val.b;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeList
 *  Description:    create a list of c elements
 * =====================================================================================
 */
FUNCTIONALITY void* N(makeList)(int c,VALUE * args){
    VALUE v;

    v.ls         = MALLOC(sizeof(struct N(List)));
    v.ls->t      = N(LIST);
    v.ls->islist = 1;
    v.ls->nargs  = c;
    v.ls->args = args;
    return v.b;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeNIL
 *  Description:    create an empty list
 * =====================================================================================
 */
FUNCTIONALITY void* N(makeNIL)(void){
    VALUE v;
    v.ls         = MALLOC(sizeof(struct N(List)));
    v.ls->t      = N(LIST);
    v.ls->nargs  = 0;
    v.ls->islist = 1;
    v.ls->args   = NULL;
    return v.b;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makePair
 *  Description:    return a pair (v,v2)
 * =====================================================================================
 */
FUNCTIONALITY void* N(makePair)(void* v,void* v2){
	VALUE * ls = MALLOC(2 * sizeof(VALUE));
	ls[0].b          = v;
	ls[1].b          = v2;
    VALUE ret;      
	ret.b = N(makeList)(2,ls);
    ret.ls->islist = 0;
    return ret.b;
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
FUNCTIONALITY void* N(makeDefine)(void* v,void* v2){
    VALUE val;
    val.d       = MALLOC(sizeof(struct N(Define)));
    val.d->t    = N(DEFINE);
    val.d->var.b  = v;
    val.d->expr.b = v2;
    return val.b;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    copyValue
 *  Description:    copy a VALUE's memory contents
 *      TODO I don't think this is needed
 * =====================================================================================
 */
/*FUNCTIONALITY VALUE N(copyValue)(VALUE par){

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
            return makeSI(par.i->arg); //TODO resolve copying of arg
        #else
        case IS : 
            return makeIS(par.i->label);
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
            DEBUG_PRINT("Could not Copy !!!");
            exit(1);
}}*/



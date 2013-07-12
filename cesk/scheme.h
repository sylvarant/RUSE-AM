/*
 * =====================================================================================
 *
 *       Filename:  scheme.h
 *
 *    Description:  The scheme language definition
 *
 *        Created:  06/28/2013 14:46:02
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */


// No protections double includes allowed
#define SCHEME_INCLUDED

#include <stdio.h>

#include "binding.h" // adds global !

/*-----------------------------------------------------------------------------
 *  Preprocessing
 *-----------------------------------------------------------------------------*/
#define SCM_(TYPE) struct N(TYPE) { \
                          enum N(Tag) t;

#define _SCM }; 
#define MEM(TYPE) VALUE TYPE;
#define MEML(TYPE) VALUE * TYPE;
#define SICM_(TYPE,N) SCM_(TYPE) \
                      MEM(N) \
                      _SCM


/*-----------------------------------------------------------------------------
 *  Tags
 *-----------------------------------------------------------------------------*/

// language elements tags
enum N(Tag) {

    // Trick to identify Unalocated VALUE's 
    N(ERROR),

    // Do nothing
    N(NOP),

    // values
    N(VOID), N(INT), N(BOOLEAN), N(CLOSURE),  N(LIST), N(QUOTE), 

    // Computation
    N(CONTINUATION), N(PRIM), N(LAM), N(IF), N(SYMBOL), N(APPLICATION), N(CALLCC), N(SET), 
    N(LET), N(LETREC), N(BEGIN), N(CAR), N(CDR), N(CONS), N(LISTQ), N(DEFINE), N(NULLQ), N(PAIRQ),

    // Boundary transition
    #ifdef SECURE
        SI
    #else
        IS
    #endif
};

// Continuationt tags
enum N(KTag) {N(KLET),N(KRET),N(KCONTINUE)};

/*-----------------------------------------------------------------------------
 * A Union of Continuations to save space
 *-----------------------------------------------------------------------------*/

typedef union N(Kont_u){
    void * empty;
    struct N(KLet) * l;  
    struct N(KRet) * r;
    struct N(KCont) * c;
}KONT;

/*-----------------------------------------------------------------------------
 * Union of Descriptors
 *-----------------------------------------------------------------------------*/

typedef union N(Value_u) {
    enum N(Tag) tt;
    struct N(Int) * z;
    struct N(Boolean) * b;
    struct N(Lambda) * l;
    struct N(Prim) * p;
    struct N(Symbol) * s;
    struct N(Application) * a;
    struct N(If) * f;
    struct N(Closure) * c;
    struct N(Continuation) * k;
    struct N(Callcc) * cc;
    struct N(SetV) * sv;
    struct N(Let) * lt;
    struct N(Letrec) * lr;
    struct N(Begin) * bg;
    struct N(Car) * car;
    struct N(Cdr) * cdr;
    struct N(Cons) * cons;
    struct N(List) * ls;
    struct N(Quote) * q;
    struct N(PairQ) * pq;
    struct N(ListQ) * lq;
    struct N(NullQ) * nq;
    struct N(Define) * d;
    #ifdef SECURE
        struct SI * i;
    #else
        struct IS * i;
    #endif
} VALUE;


// Type of Primitive operation
typedef void* (* N(PrimOp)) (void*,void*);

/*-----------------------------------------------------------------------------
 * Descriptors
 *-----------------------------------------------------------------------------*/

SCM_(Int)
    int value;
_SCM

SCM_(Boolean)
    unsigned int value;
_SCM

SCM_(Lambda)
    int nargs;
    MEM(body)
    MEML(arguments) 
_SCM



SCM_(Prim)
    N(PrimOp) exec; 
    int nargs;
    MEML(arguments)
_SCM

SCM_(If)
    MEM(cond)
    MEM(cons)
    MEM(alt)
_SCM

SCM_(Symbol)
    char * name;  
_SCM

SCM_(Application)
    int nargs;
    MEML(arguments)
_SCM

SCM_(Closure)
    MEM(lambda)
    BINDING * env;
_SCM

SCM_(Continuation)
    KONT kstar;
_SCM

SICM_(Callcc,function)

SCM_(SetV)
    MEM(var)
    MEM(value)
_SCM

SCM_(Let)
    MEM(var)
    MEM(expr)
    MEM(body)
_SCM

SCM_(Letrec)
    int nargs;
    MEM(body)
    MEML(vars)
    MEML(exprs)
_SCM

SCM_(Begin)
    int nargs;
    MEML(stmts)
_SCM

SICM_(Car,arg)

SICM_(Cdr,arg)

SCM_(Cons)
    MEM(arg)
    MEM(arg2)
_SCM

SCM_(Define)
    MEM(var)
    MEM(expr)
_SCM

SICM_(Quote,arg)

SICM_(PairQ,arg)

SICM_(ListQ,arg)

SICM_(NullQ,arg)

SCM_(List)
    unsigned int islist;
    int nargs;
    MEML(args)
_SCM

#ifdef SECURE
struct SI{
    enum N(Tag) t;
    OTHERVALUE arg;
};
#else
struct IS{
    enum N(Tag) t;
    int label;
};
#endif
  
/*-----------------------------------------------------------------------------
 * Continuation Structure Definitions
 *-----------------------------------------------------------------------------*/

struct N(KRet){
    enum N(KTag) t;
    KONT next; 
};

struct N(KCont){
    enum N(KTag) t;
	BINDING * e;
    KONT next; 
};

struct N(KLet){
    enum N(KTag) t;
    VALUE var;
	VALUE expr;
	BINDING * e;
    KONT next; 
};


/*-----------------------------------------------------------------------------
 *  Functionality -> Local to scheme, entrypoints are in global.h
 *-----------------------------------------------------------------------------*/

// constructors - these functions cross spm boundaries -> pass value as pointer
FUNCTIONALITY void * N(makeInt)(int); 
FUNCTIONALITY void * N(makeBoolean)(unsigned int);
FUNCTIONALITY void * N(makeIf)(void *,void *,void *);
FUNCTIONALITY void * N(makeLambda)(int,void *,VALUE*);
FUNCTIONALITY void * N(makePrim)(int,N(PrimOp),VALUE*);
FUNCTIONALITY void * N(makeSymbol)(char *);
FUNCTIONALITY void * N(makeApplication)(int,VALUE*);
FUNCTIONALITY void * N(makeCallcc)(void *);
FUNCTIONALITY void * N(makeSet)(void *,void *);
FUNCTIONALITY void * N(makeLet)(void *,void *,void *);
FUNCTIONALITY void * N(makeLetrec)(int,void *,VALUE*);
FUNCTIONALITY void * N(makeVoid)(void);
FUNCTIONALITY void * N(makeNop)(void);
FUNCTIONALITY void * N(makeBegin)(int,VALUE*);
FUNCTIONALITY void * N(makeCar)(void *);
FUNCTIONALITY void * N(makeCdr)(void *);
FUNCTIONALITY void * N(makeCons)(void *,void *);
FUNCTIONALITY void * N(makeList)(int,VALUE*);
FUNCTIONALITY void * N(makeNIL)(void);
FUNCTIONALITY void * N(makePair)(void *,void *);
FUNCTIONALITY void * N(makeQuote)(void *);
FUNCTIONALITY void * N(makePairQ)(void *);
FUNCTIONALITY void * N(makeListQ)(void *);
FUNCTIONALITY void * N(makeNullQ)(void *);
FUNCTIONALITY void * N(makeDefine)(void *,void *);
FUNCTIONALITY void * N(makeContinuation)(KONT);
FUNCTIONALITY void * N(makeClosure)(void *,BINDING*);

// continuations
FUNCTIONALITY KONT N(makeKLet)(VALUE,VALUE,BINDING*,KONT);
FUNCTIONALITY KONT N(makeKCont)(BINDING*,KONT);
FUNCTIONALITY KONT N(makeKRet)(KONT);

// boundary
#ifdef SECURE
FUNCTIONALITY void * makeSI(void *);
#else
FUNCTIONALITY void * makeIS(int);
#endif

// memory duplication
//FUNCTIONALITY void * N(copyValue)(void *);

// math TODO probably not language specific
FUNCTIONALITY void* N(sumPrim)(void*,void*);
FUNCTIONALITY void* N(differencePrim)(void*,void*);
FUNCTIONALITY void* N(productPrim)(void*,void*);
FUNCTIONALITY void* N(numequalPrim)(void*,void*);


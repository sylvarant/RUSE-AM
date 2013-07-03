#ifndef SCHEME_INCLUDED
#define SCHEME_INCLUDED

#include <stdbool.h>
#include <stdio.h>
#include "environment.h"

/*-----------------------------------------------------------------------------
 *  Macro's
 *-----------------------------------------------------------------------------*/
#define SCM_(TYPE) struct TYPE { \
                          enum Tag t;

#define _SCM }; 
#define MEM(TYPE) union Value TYPE;
#define MEML(TYPE) union Value * TYPE;
#define SICM_(TYPE,N) SCM_(TYPE) \
                      MEM(N) \
                      _SCM

#ifdef DEBUG
#include <assert.h>
# define DEBUG_PRINT(x) printf("DEBUG:: "); printf x ; printf("\n");fflush(stdout);
#else
# define DEBUG_PRINT(x) do {} while (0);
#endif


/*-----------------------------------------------------------------------------
 *  Tags
 *-----------------------------------------------------------------------------*/
enum Tag {VOID, INT, BOOLEAN, CLOSURE, CONTINUATION, PRIM, LAM, IF, SYMBOL, APPLICATION, CALLCC, SET, LET, LETREC,
BEGIN, CAR, CDR, CONS, LIST, QUOTE, PAIRQ, LISTQ, UNDEF, IS, DEFINE,NULLQ} ;

enum KTag {KLET,KRET,KCONTINUE};


// the primitive operations
union Value;
typedef union Value (* PrimOp) (union Value,union Value);

/*-----------------------------------------------------------------------------
 * Preset for Language structure definitions
 *-----------------------------------------------------------------------------*/

typedef union kont_u{
    void * empty;
    struct let_kont * l;  
    struct ret_kont * r;
    struct cont_kont * c;
}kont;


/*-----------------------------------------------------------------------------
 * Language Structure Definitions
 *-----------------------------------------------------------------------------*/

union Value {
    enum Tag tt;
    struct Int * z;
    struct Boolean * b;
    struct Lambda * l;
    struct Prim * p;
    struct Symbol * s;
    struct Application * a;
    struct If * f;
    struct Closure * c;
    struct Continuation * k;
    struct Callcc * cc;
    struct SetV * sv;
    struct Let * lt;
    struct Letrec * lr;
    struct Begin * bg;
    struct Car * car;
    struct Cdr * cdr;
    struct Cons * cons;
    struct List * ls;
    struct Quote * q;
    struct PairQ * pq;
    struct ListQ * lq;
    struct NullQ * nq;
    struct Define * d;
    struct IS * i;
};

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
    PrimOp exec; 
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
    environ  * env;
_SCM

SCM_(Continuation)
    kont kstar;
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
    int islist;
    int nargs;
    MEML(args)
_SCM

SCM_(IS)
    int label;
_SCM
  

/*-----------------------------------------------------------------------------
 * Continuation Structure Definitions
 *-----------------------------------------------------------------------------*/

struct ret_kont{
    enum KTag t;
    kont next; 
};

struct cont_kont{
    enum KTag t;
	environ * e;
    kont next; 
};

struct let_kont{
    enum KTag t;
	union Value var;
	union Value expr;
	environ * e;
    kont  next; 
};


/*-----------------------------------------------------------------------------
 *  Functions
 *-----------------------------------------------------------------------------*/

// simplifications
typedef union Value Value ;

// functionality
extern Value evaluate(Value p);
extern char * toString(Value,bool);
extern void freevalue(Value *);
extern Value copyvalue(Value par);

// primitive operators
extern Value sumPrim(Value,Value);
extern Value differencePrim(Value,Value);
extern Value productPrim(Value,Value);
extern Value numequalPrim(Value,Value);

// constructors
extern Value makeInt(int n); 
extern Value makeBoolean(unsigned int b);
extern Value makeIf(Value a,Value b,Value c);
extern Value makeLambda(int,Value body,...);
extern Value makePrim(int,PrimOp,Value arg,...);
extern Value makeSymbol(char *);
extern Value makeApplication(int c,Value a,...);
extern Value makeCallcc(Value f);
extern Value makeSet(Value v,Value t);
extern Value makeLet(Value v,Value t,Value b);
extern Value makeLetrec(int c,Value v,Value t,...);
extern Value makeVoid(void);
extern Value makeBegin(int c,Value v,...);
extern Value makeCar(Value v);
extern Value makeCdr(Value v);
extern Value makeCons(Value v,Value v2);
extern Value makeList(int c,Value v,...);
extern Value makeNIL(void);
extern Value makePair(Value v,Value v2);
extern Value makeQuote(Value v);
extern Value makePairQ(Value v);
extern Value makeListQ(Value v);
extern Value makeNullQ(Value v);
extern Value makeDefine(Value v,Value v2);
extern Value makeIS(int label);

// Internal magic
extern Value makeContinuation(kont kstar);
extern Value makeClosure(Value atom, environ * htbl);
extern Value makeUndef(void);

// get input - todo should this be here ?
extern Value * getinput();
extern int getinput_n();

#endif


#ifndef SCHEMES_INCLUDED
#define SCHEMES_INCLUDED

#include <stdbool.h> // TODO if SPM ?
#include <stdio.h>
#include "senvironment.h"
#include "scheme.h" // TODO resolve

/*-----------------------------------------------------------------------------
 *  Macro's
 *-----------------------------------------------------------------------------*/
#define SCM_(TYPE) struct TYPE { \
                          enum STag t;

#define _SCM }; 
#define MEM(TYPE) union SValue TYPE;
#define MEML(TYPE) union SValue * TYPE;
#define SICM_(TYPE,N) SCM_(TYPE) \
                      MEM(N) \
                      _SCM



/*-----------------------------------------------------------------------------
 *  STags
 *-----------------------------------------------------------------------------*/
enum STag {SVOID, SINT, SBOOLEAN, SCLOSURE, SCONTINUATION, SPRIM, SLAM, SIF, SSYMBOL, SAPPLICATION, 
SCALLCC, SSET, SLET, SLETREC,
SBEGIN, SCAR, SCDR, SCONS, SLIST, SQUOTE, SPAIRQ, SI, SLISTQ, SUNDEF, SDEFINE,SNULLQ} ;

enum KSTag {SKLET,SKRET,SKCONTINUE};

// the primitive operations
union SValue;
typedef union SValue (* SPrimOp) (union SValue,union SValue);


/*-----------------------------------------------------------------------------
 * Preset for Language structure definitions
 *-----------------------------------------------------------------------------*/

typedef union skont_u{
    void * empty;
    struct slet_kont * l;  
    struct sret_kont * r;
    struct scont_kont * c;
}skont;


/*-----------------------------------------------------------------------------
 * Language Structure Definitions
 *-----------------------------------------------------------------------------*/

union SValue {
    enum STag tt;
    struct SInt * z;
    struct SBoolean * b;
    struct SLambda * l;
    struct SPrim * p;
    struct SSymbol * s;
    struct SApplication * a;
    struct SIf * f;
    struct SClosure * c;
    struct SContinuation * k;
    struct SCallcc * cc;
    struct SSetV * sv;
    struct SLet * lt;
    struct SLetrec * lr;
    struct SBegin * bg;
    struct SCar * car;
    struct SCdr * cdr;
    struct SCons * cons;
    struct SList * ls;
    struct SQuote * q;
    struct SPairQ * pq;
    struct SListQ * lq;
    struct SNullQ * nq;
    struct SDefine * d;
    struct SI * i;
};

SCM_(SInt)
    int value;
_SCM

SCM_(SBoolean)
    unsigned int value;
_SCM

SCM_(SLambda)
    int nargs;
    MEM(body)
    MEML(arguments) 
_SCM

SCM_(SPrim)
    SPrimOp exec; 
    int nargs;
    MEML(arguments)
_SCM

SCM_(SIf)
    MEM(cond)
    MEM(cons)
    MEM(alt)
_SCM

SCM_(SSymbol)
    char * name;  
_SCM

SCM_(SApplication)
    int nargs;
    MEML(arguments)
_SCM

SCM_(SClosure)
    MEM(lambda)
    N(environ)  * env;
_SCM

SCM_(SContinuation)
    skont kstar;
_SCM

SICM_(SCallcc,function)

SCM_(SSetV)
    MEM(var)
    MEM(value)
_SCM

SCM_(SLet)
    MEM(var)
    MEM(expr)
    MEM(body)
_SCM

SCM_(SLetrec)
    int nargs;
    MEM(body)
    MEML(vars)
    MEML(exprs)
_SCM

SCM_(SBegin)
    int nargs;
    MEML(stmts)
_SCM

SICM_(SCar,arg)

SICM_(SCdr,arg)

SCM_(SCons)
    MEM(arg)
    MEM(arg2)
_SCM

SCM_(SDefine)
    MEM(var)
    MEM(expr)
_SCM

SICM_(SQuote,arg)

SICM_(SPairQ,arg)

SICM_(SListQ,arg)

SICM_(SNullQ,arg)

SCM_(SList)
    int islist;
    int nargs;
    MEML(args)
_SCM

SCM_(SI)
    Value arg;
_SCM
  

/*-----------------------------------------------------------------------------
 * Continuation Structure Definitions
 *-----------------------------------------------------------------------------*/

struct sret_kont{
    enum KSTag t;
    skont next; 
};

struct scont_kont{
    enum KSTag t;
	N(environ) * e;
    skont next; 
};

struct slet_kont{
    enum KSTag t;
	union SValue var;
	union SValue expr;
	N(environ) * e;
    skont  next; 
};


/*-----------------------------------------------------------------------------
 *  Functions
 *-----------------------------------------------------------------------------*/
typedef struct sfunctions_t{
    int label;  
    struct sfunctions_t * next;  
}sfunctions;


// simplifications
typedef union SValue SValue ;

// TODO Remove
FUNCTIONALITY SValue N(prim_sum)(SValue,SValue);
FUNCTIONALITY SValue N(prim_difference)(SValue,SValue);
FUNCTIONALITY SValue N(prim_product)(SValue,SValue);
FUNCTIONALITY SValue N(prim_numEqual)(SValue,SValue);

#endif

/*
 * =====================================================================================
 *
 *       Filename:  insecure_macro.h
 *
 *    Description:  All insecure macro trickery
 *
 *        Created:  07/03/2013 16:36:02
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#ifndef INSECURE_MACRO_INCLUDED
#define INSECURE_MACRO_INCLUDED

#include "spm.h"

/*-----------------------------------------------------------------------------
 *  Modifiers
 *-----------------------------------------------------------------------------*/

// These are needed to deal with SANCUS
#define LOCAL static
#define SECRET_DATA static
#define FUNCTIONALITY extern


/*-----------------------------------------------------------------------------
 *  Naming
 *-----------------------------------------------------------------------------*/

// add S to methods in this case
#define N(NAME) NAME

// add nothing to access the Other
#define OTHERN(NAME) S##NAME

// Secure uses SVALUE
#define VALUE N(Value)
#define OTHERVALUE OTHERN(Value)

// Secure uses SBinding
#define BINDING N(Binding)
#define OTHERBINDING OTHERN(Binding)

// Secure uses SKont
#define KONT N(Kont)
#define OTHERKONT OTHERN(Kont)

// Secure uses SType
#define TYPE N(Type)
#define OTHERTYPE OTHERN(Type)

// Secure uses SLimbo
#define LIMBO N(Limbo)
#define OTHERLIMBO OTHERN(Limbo)

// Secure uses SState
#define STATE N(State)
#define OTHERSTATE OTHERN(State)


/*-----------------------------------------------------------------------------
 *  Memory
 *-----------------------------------------------------------------------------*/

#define MALLOC malloc // TODO adapt
#define OTHERMALLOC malloc


#endif

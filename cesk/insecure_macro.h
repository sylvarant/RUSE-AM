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

/*-----------------------------------------------------------------------------
 *  Modifiers
 *-----------------------------------------------------------------------------*/

// These are needed to deal with SANCUS
#define LOCAL static
#define SECRET_DATA static
#define FUNCTIONALITY extern
#define HOOK extern


// Entry point is sacred
#ifdef SANCUS_SPM
    #include <sancus/sm_support.h>
    #define __MSP430_INTRINSICS_H_
    #include <msp430.h>

    #define SPM_NAME "Spm"
    #define ENTRYPOINT SM_ENTRY(SPM_NAME) extern

#else

#define ENTRYPOINT extern

#endif


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

/*
 * =====================================================================================
 *
 *       Filename:  secure_macro.h
 *
 *    Description:  All secure macro trickery
 *
 *        Created:  06/27/2013 14:38:32
 *
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#ifndef SECURE_MACRO_INCLUDED
#define SECURE_MACRO_INCLUDED


/*-----------------------------------------------------------------------------
 *  Modifiers
 *-----------------------------------------------------------------------------*/

#ifdef SANCUS_SPM

    // When using the SPM 

    #include <sancus/sm_support.h>
    #define __MSP430_INTRINSICS_H_
    #include <msp430.h>
    
    // The SPM Name
    // TODO set with argument
    #define SPM_NAME "Spm"

    #define LOCAL SM_FUNC(SPM_NAME) static
    #define SECRET_DATA SM_DATA(SPM_NAME) static 
    #define FUNCTIONALITY SM_FUNC(SPM_NAME) extern
    #define ENTRYPOINT SM_ENTRY(SPM_NAME) extern

#else 

    // Default
    #define LOCAL static
    #define SECRET_DATA static
    #define FUNCTIONALITY extern
    #define ENTRYPOINT extern

#endif


/*-----------------------------------------------------------------------------
 *  Naming
 *-----------------------------------------------------------------------------*/

// add S to methods in this case
#define N(NAME) S##NAME

// add nothing to access the Other
#define OTHERN(NAME) NAME

// Secure uses SVALUE
#define VALUE N(Value)
#define OTHERVALUE Value

// Secure uses SBinding
#define BINDING N(Binding)
#define OTHERBINDING Binding

// Secure uses SKont
#define KONT N(Kont)
#define OTHERKONT Kont

// Secure uses SLimbo
#define LIMBO N(Limbo)
#define OTHERLIMBO Limbo

// Secure uses SState
#define STATE N(State)
#define OTHERSTATE State


/*-----------------------------------------------------------------------------
 *  Memory
 *-----------------------------------------------------------------------------*/

#define MALLOC malloc // TODO adapt
#define OTHERMALLOC malloc

#endif


/*
 * =====================================================================================
 *
 *       Filename:  spm.h
 *
 *    Description:  Define the SPM used by the SECURE machine
 *
 *        Created:  07/12/2013 14:51:27
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#ifndef SPM_IDENTIFICATION
#define SPM_IDENTIFICATION

/*-----------------------------------------------------------------------------
 * SANCUS SPM structure
 *-----------------------------------------------------------------------------*/

#ifdef SANCUS_SPM

    #include <sancus/sm_support.h>
    #define __MSP430_INTRINSICS_H_
    #include <msp430.h>

    #define SPM_NAME "secure_vm"

    extern struct SancusModule secure_vm;

    // Entry point is sacred
    #define ENTRYPOINT SM_ENTRY(SPM_NAME) extern

#else
    
    #define ENTRYPOINT extern

#endif

// An entrypoints counter part
#define HOOK extern

#endif


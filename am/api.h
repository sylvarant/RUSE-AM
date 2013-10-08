/*
 * =====================================================================================
 *
 *       Filename:  api.h
 *
 *    Description:  C API to the RUSE AM
 *                  Note::  that insecure means in unprotected memory space while
 *                          secure means the opposite
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#ifndef RUSE_API_H
#define RUSE_API_H


/*--------------------------------------------------------------------------------------
 *                      Insecure Methods 
 *-------------------------------------------------------------------------------------*/

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    evaluate
 *
 *     Argument:    <code> 
 *                  Bytes convertible into VALUE structs that are to be executed
 *                  by the evaluator
 *
 *  Description:    executes the Ruse Ml/Scheme code that the bytes represent
 *
 * Precondition:    If the following preconditions is violated the RUSE AM shuts down :
 *                      - code must be convertible into a valid VALUE
 *
 *       Result:    A Ruse ML / Scheme VALUE in bytes in unprotected memory
 * =====================================================================================
 */
extern void * evaluate(void * code); 


/*--------------------------------------------------------------------------------------
 *                      Secure Entrypoints 
 *-------------------------------------------------------------------------------------*/

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    secure_eval
 *
 *     Argument:    <label> 
 *                  A label to a Ruse ML/Scheme function that was either: 
 *                      - statically defined
 *                      - dynamically released
 *
 *  Description:    executes the Ruse Ml/Scheme function that the label points to
 *
 * Precondition:    If either of the following preconditions are violated the 
 *                  RUSE AM shuts down :
 *                      - sload must be executed
 *                      - Argument must  
 *
 *       Result:    A Ruse ML / Scheme VALUE in bytes in unprotected memory
 * =====================================================================================
 */
extern void * secure_eval(int label);


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    sload
 *
 *     Argument:    <name>
 *                  The name of a secure program. This value is currently ignored :
 *                      - It's best to just pass NULL
 *
 *                  <callback>
 *                  The evaluator to be called by the secure_eval method when wanting 
 *                  to compute insecure code.
 *
 *  Description:   setup the machine  
 *
 * =====================================================================================
 */
extern void sload(char * name,void * (*callback)(void *));

#endif

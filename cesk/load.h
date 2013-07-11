/*
 * =====================================================================================
 *
 *       Filename:  load.h
 *
 *    Description:  load a binary file into one of the machines
 *
 *        Created:  07/09/2013 16:27:05
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#ifndef LOAD_INCLUDED
#define LOAD_INCLUDED


#include "cesk.h" 

/*-----------------------------------------------------------------------------
 *  Functionality
 *-----------------------------------------------------------------------------*/
FUNCTIONALITY VALUE N(readCode)(char***);
FUNCTIONALITY VALUE * N(readByteCode) (char*,int *);

#endif

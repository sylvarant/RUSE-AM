/*
 * =====================================================================================
 *
 *       Filename:  string.h
 *
 *    Description:  To string all CESK descriptors
 *
 *        Created:  06/27/2013 15:04:14
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

// Only Strings in Secure when debugging
#if defined(DEBUG) || defined(INSECURE)

#ifndef STRING_INCLUDED
#define STRING_INCLUDED

#include "scheme.h" // TODO general file

/*-----------------------------------------------------------------------------
 *  Functionality
 *-----------------------------------------------------------------------------*/
FUNCTIONALITY char * N(toString) (VALUE,bool);

#endif
#endif

/*
 * =====================================================================================
 *
 *       Filename:  type.h
 *
 *    Description:  type checking
 *
 *        Created:  07/23/2013 16:26:16
 *
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#ifdef SECURE
#define TYPING_INCLUDED

#include "global.h"


/*-----------------------------------------------------------------------------
 *  Functionality
 *-----------------------------------------------------------------------------*/
FUNCTIONALITY void * N(typecheck)(void *);
FUNCTIONALITY void * OTHERN(typecheck)(void *);

#endif
#endif

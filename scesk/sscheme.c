/*
 * =====================================================================================
 *
 *       Filename:  sscheme.c
 *
 *    Description:  Scheme Language descriptors implementation
 *
 *        Created:  07/01/2013 19:30:53
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#include "sscheme.h" // TODO  generalize


/*-----------------------------------------------------------------------------
 *  Preprocessor
 *-----------------------------------------------------------------------------*/

// Compress similar make definitions
#define MAKE_(TYPE,TAG,ID,ARG) FUNCTIONALITY SValue Make##TYPE(SValue v){\
    SValue val;\
    val.ID      = (struct TYPE *) malloc(sizeof(struct TYPE));\
    val.ID->t       = TAG;\
    val.ID->ARG = v; \
    return val;\
}

// Compress similar copy definitions
#define COPY_(TYPE,TAG,ID,ARG,AA){\
    SValue out;\
    out.ID = (struct TYPE *) malloc(sizeof(struct TYPE));\
    out.ID->t     = TAG;\
    out.ID->nargs = ARG.ID->nargs;\
    out.ID->AA = (SValue *) malloc( ARG.ID->nargs * (sizeof (SValue)));\
    for(int i = 0; i < ARG.ID->nargs; i++){\
            out.ID->AA[i] = scopyvalue(ARG.ID->AA[i]);}\
    return out;}



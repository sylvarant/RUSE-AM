/*
 * =====================================================================================
 *
 *       Filename:  label.h
 *
 *    Description:  The Labelling functionality of the secure machine
 *
 *        Created:  07/02/2013 16:41:23
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#ifdef SECURE

#include "global.h"

#ifndef LABEL_INCLUDED
#define LABEL_INCLUDED

/*-----------------------------------------------------------------------------
 *  List of labels -- labels are integers
 *-----------------------------------------------------------------------------*/
typedef struct Label_t{
    int label;  
    struct Label_t * next;  
}Label;


/*-----------------------------------------------------------------------------
 *  Functionality
 *-----------------------------------------------------------------------------*/
FUNCTIONALITY void insertLabel(Label **,int);
FUNCTIONALITY unsigned int hasLabel(Label *,int);

#endif
#endif

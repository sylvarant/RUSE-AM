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

#ifndef LABEL_INCLUDED
#define LABEL_INCLUDED

#include "ruse_all.h"

/*-----------------------------------------------------------------------------
 * Label annotations
 *-----------------------------------------------------------------------------*/
typedef struct Annotation_t{
	TYPE ty;		
	VALUE t;			
}ANNOTATION;

/*-----------------------------------------------------------------------------
 *  List of labels -- labels are integers -> data
 *-----------------------------------------------------------------------------*/
typedef struct Label_t{
    int label;  
	ANNOTATION * an;	
    struct Label_t * next;  
}LABEL;


/*-----------------------------------------------------------------------------
 *  Functionality
 *-----------------------------------------------------------------------------*/
FUNCTIONALITY void N(insertLabel)(LABEL **,int,ANNOTATION*);
FUNCTIONALITY ANNOTATION * N(hasLabel)(LABEL *,int);
FUNCTIONALITY int N(newLabel)(void);

#endif
#endif

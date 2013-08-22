/*
 * =====================================================================================
 *
 *       Filename:  label.c
 *
 *    Description:  The Labelling functionality of the secure machine
 *					A simple list implemenation
 *
 *        Created:  07/02/2013 16:58:13
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#include "label.h"

#ifdef LABEL_INCLUDED

#include <stdlib.h>

/*-----------------------------------------------------------------------------
 *  local variables
 *-----------------------------------------------------------------------------*/
 LOCAL int free_label = 0;


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    hasLabel
 *  Description:    return 1 of it has a certain label
 * =====================================================================================
 */
FUNCTIONALITY int N(newLabel)(){
	return free_label++;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    hasLabel
 *  Description:    return 1 of it has a certain label
 * =====================================================================================
 */
FUNCTIONALITY ANNOTATION * N(hasLabel)(LABEL * ls,int label){
    LABEL * node = ls;

    while(node){
        if(node->label == label){
            return node->an;
        }
        node = node->next;
    }

    return NULL;       
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    insertLabel
 *  Description:    add a label to the list of labels
 * =====================================================================================
 */
FUNCTIONALITY void N(insertLabel)(LABEL ** ls,int label,ANNOTATION * an){
    LABEL * node = MALLOC(sizeof(LABEL));
    node->next   = *ls;
    node->label  = label;
	node->an     = an; 
    *ls          = node;
}

#endif

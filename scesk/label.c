/*
 * =====================================================================================
 *
 *       Filename:  label.c
 *
 *    Description:  The Labelling functionality of the secure machine
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

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    hasLabel
 *  Description:    return 1 of it has a certain label
 * =====================================================================================
 */
FUNCTIONALITY unsigned int hasLabel(Label * ls,int label){
    Label * node = ls;

    while(node){
        if(node->label == label){
            return 1;
        }
        node = node->next;
    }

    return 0;       
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    insertLabel
 *  Description:    add a label to the list of labels
 * =====================================================================================
 */
FUNCTIONALITY void insertLabel(Label ** ls,int label){
    Label * node = MALLOC(sizeof(Label));
    node->next   = *ls;
    node->label  = label;
    *ls          = node;
}

#endif

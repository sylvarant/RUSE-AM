/*
 * =====================================================================================
 *
 *       Filename:  binding.c
 *
 *    Description:  Implementation of the environment
 *
 *        Created:  06/28/2013 15:28:39
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#include "cesk.h"
#include <stdlib.h>
#include <string.h>


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    getBinding
 *  Description:    return an element from a given environment for a given key
 * =====================================================================================
 */
FUNCTIONALITY VALUE * N(getBinding)(BINDING * ls,char * key)
{
    BINDING * node = ls;
    while(node) {
        if(strcmp(key,node->key) == 0)
            return node->address;
        node = node->next;
    }
    return NULL;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    insertBinding
 *  Description:    add a new binding to the environment
 * =====================================================================================
 */
FUNCTIONALITY VALUE * N(insertBinding)(BINDING ** head,char * key)
{
    BINDING * node = MALLOC(sizeof(BINDING));

    if(*head == NULL){
        *head        = node;
        (*head)->next = NULL;
    }else{
        node->next = (*head);
        (*head)    = node;
    }
    node->key = key;
    node->address = MALLOC(sizeof(VALUE));
	node->address->tt = N(ERROR);

	return node->address;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    copyEnvironment
 *  Description:    copy the environment
 *					Deprecated !!
 * =====================================================================================
 */
/*
FUNCTIONALITY BINDING * N(copyBinding)(BINDING * ls){

    if(ls ==  NULL) return NULL;

    BINDING * new  = MALLOC(sizeof(BINDING));
    BINDING * ret  = new;
    BINDING * node = ls;

    while(1) {
        new->key   = node->key;
        new->value = node->value;

        if(node->next != NULL){
            new->next = MALLOC(sizeof(BINDING)); 
            node      = node->next;
            new       = new->next;
        } else{ 
            new->next = NULL;
            break;
        }
    }

    return ret;
}
*/

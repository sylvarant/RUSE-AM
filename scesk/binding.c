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

#include "binding.h" 
#include <stdlib.h>


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    getBinding
 *  Description:    return an element from a given environment for a given key
 * =====================================================================================
 */
FUNCTIONALITY int N(getBinding)(BINDING * ls, const char * key)
{
    BINDING * node = ls;
    while(node) {
        if(strcmp(key,node->key) == 0)
            return node->value;
        node = node->next;
    }
    return (-1);
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    insertBinding
 *  Description:    add something to environment
 * =====================================================================================
 */
FUNCTIONALITY void N(insertBinding)(BINDING ** ls,char * key,int value)
{
    BINDING * node;
    BINDING ** tmp = ls;

    while(*tmp) {
        if(strcmp(key,(*tmp)->key) == 0)
            break;
        tmp = &(*tmp)->next;
    }

    if(*tmp) { 
        node = *tmp;
    } else {
        node = MALLOC(sizeof *node);
        node->next = NULL;
        *tmp = node;
    }

    node->key = key;
    node->value = value;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    copyEnvironment
 *  Description:    copy the environment
 * =====================================================================================
 */
FUNCTIONALITY BINDING * N(copyBinding)(BINDING * ls){

    if(ls ==  NULL) return NULL;

    BINDING * new =  MALLOC(sizeof(BINDING));
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

    return new;
}


/*
 * =====================================================================================
 *
 *       Filename:  senvironment.c
 *
 *    Description:  The environment structure
 *
 *        Created:  06/28/2013 15:28:39
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#include "senvironment.h" // TODO general
#include <stdlib.h>


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    get
 *  Description:    return an element from a given environment for a given key
 * =====================================================================================
 */
FUNCTIONALITY int N(get)(N(environ) * table, const char * key)
{
    struct N(envnode) *node;
    node = table->bucket;
    while(node) {
        if(strcmp(key,node->key) == 0)
            return node->value;
        node = node->next;
    }
    return (-1);
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    insert
 *  Description:    add something to environment
 * =====================================================================================
 */
FUNCTIONALITY int N(insert)(N(environ) *table,char * key,int value)
{
    struct N(envnode) **tmp;
    struct N(envnode) *node ;

    tmp = &table->bucket;

    while(*tmp) {
        if(strcmp(key,(*tmp)->key) == 0)
            break;
        tmp = &(*tmp)->next;
    }

    if(*tmp) { 
        node = *tmp;
    } else 
    {
        node = MALLOC(sizeof *node);
        if(node == NULL) return -1;
        node->next = NULL;
        *tmp = node;
        table->size++;
    }
    node->key = key;
    node->value = value;

    return 0;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    scopyenv
 *  Description:    copy the env
 * =====================================================================================
 */
FUNCTIONALITY N(environ) * N(copyenv)(N(environ) * table){

    N(environ) * new = (N(environ) *) MALLOC(sizeof(N(environ)));
    new->size = table->size;

    struct N(envnode) *node;
    struct N(envnode) *nnode;
    node = table->bucket;
    new->bucket = MALLOC(sizeof(struct N(envnode))); 

    nnode =  new->bucket;
	struct N(envnode) * dumb = nnode;
    while(node != NULL) {
        nnode->key             = node->key;
        nnode->value           = node->value;
        nnode->next            = MALLOC(sizeof(struct N(envnode))); 
        node = node->next;
		dumb = nnode;
        nnode = nnode->next;
    }

	if(dumb != nnode){
		dumb->next = NULL;
	}else{
		new->bucket = NULL;
	}

    free(nnode);
    return new;
}


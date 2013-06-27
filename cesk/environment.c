/*
 * =====================================================================================
 *
 *       Filename:  environment.c
 *
 *    Description: Used to be a hash table
 *
 *        Version:  1.0
 *        Created:  10/17/2012 14:54:40
 * =====================================================================================
 */

#include <stdio.h>
#define DEBUG

#ifdef DEBUG
#include <assert.h>
# define DEBUG_PRINT(x) printf("DEBUG:: "); printf x ; printf("\n");fflush(stdout);
#else
# define DEBUG_PRINT(x) do {} while (0)
#endif

#include "environment.h"

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    get
 *  Description:    return an element 
 * =====================================================================================
 */
extern int get(environ *table,const char *key)
{
    struct envnode *node;
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
extern int insert(environ *table,char *key,int value)
{
    struct envnode **tmp;
    struct envnode *node ;

    tmp = &table->bucket;
    while(*tmp) {
        if(strcmp(key,(*tmp)->key) == 0)
            break;
        tmp = &(*tmp)->next;
    }
    if(*tmp) { 
        node = *tmp;
    } else {
        node = malloc(sizeof *node);
        if(node == NULL)
            return -1;
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
 *         Name:    emptyenv
 *  Description:    clear out the memory
 * =====================================================================================
 */
extern void emptyenv(environ * table){

    struct envnode *node;
    struct envnode *nnode;
    node = table->bucket;
    nnode = node;

    while(node) {
        node = node->next;
        free(nnode);
        nnode = node;
    }
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    copyenv
 *  Description:    copy the env
 * =====================================================================================
 */
extern environ * copyenv(environ * table){
    environ * new = (environ *) malloc(sizeof(environ));
    new->size = table->size;

    struct envnode *node;
    struct envnode *nnode;
    node = table->bucket;
    new->bucket = (struct envnode *) malloc(sizeof(struct envnode)); 

    nnode =  new->bucket;
	struct envnode * dumb = nnode;
    while(node != NULL) {
        nnode->key             = node->key;
        nnode->value           = node->value;
        nnode->next            = (struct envnode *) malloc(sizeof(struct envnode)); 
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

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    keylist
 *  Description:    return a list of all keys
 * =====================================================================================
 */
extern struct keylist * keylist(environ * tbl){
    struct keylist * kl = (struct keylist *) malloc(sizeof(struct keylist));

    struct envnode *node;
    node = tbl->bucket;

    while(node) {
        kl->key      = node->key;
        kl->next  = (struct keylist *) malloc(sizeof(struct keylist));
        node = node->next;
        kl = kl->next;
    }
    free(kl);
    kl = NULL;

    return kl;
}



/*
 * =====================================================================================
 *
 *       Filename:  environment.h
 *
 *    Description:  Used to be a table
 *
 * =====================================================================================
 */

#ifndef TABLE_INCLUDED
#define TABLE_INCLUDED

#include <stdlib.h>
#include <string.h>

#define NR_BUCKETS 1024

/*-----------------------------------------------------------------------------
 *  Types
 *-----------------------------------------------------------------------------*/
struct envnode {
    char *key;
    int value;
    struct envnode *next;

};

struct keylist{
    char * key;
    struct keylist * next;
};

typedef struct environ_t {
    struct envnode *bucket;
    int size;
}environ;

/*-----------------------------------------------------------------------------
 *  Functions
 *-----------------------------------------------------------------------------*/
extern int insert(environ *table,char *key,int value);
extern int get(environ *table,const char *key);
extern void emptyenv(environ * table);
extern environ * copyenv(environ * table);
extern struct keylist * keylist(environ * table); // needed ?

#endif

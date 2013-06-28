/*
 * =====================================================================================
 *
 *       Filename:  garbage_collection.h
 *
 *    Description:  Functions to clear language descriptors 
 *
 *        Version:  1.0
 *        Created:  06/28/2013 11:18:06
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Dr. Fritz Mehner (mn), mehner@fh-swf.de
 *        Company:  FH Südwestfalen, Iserlohn
 *
 * =====================================================================================
 */

#ifndef GARBAGE_COLLECTION_INCLUDED
#define GARBAGE_COLLECTION_INCLUDED

#include "sscheme.h" // TODO general file

/*-----------------------------------------------------------------------------
 *  Functionality
 *-----------------------------------------------------------------------------*/
FUNCTIONALITY void N(freevalue)(VALUE * par);

#endif

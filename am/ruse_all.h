/*
 * =====================================================================================
 *
 *       Filename:  ruse_all.h
 *
 *    Description:  Include both the insecure and secure aspects of ruse
 *
 *        Created:  08/02/2013 18:01:21
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#ifndef RUSE_ALL_INCLUDED
#define RUSE_ALL_INCLUDED

// extra safety check -- only the secure needs access to both
#ifdef SECURE

// load insecure header
#include "undefine.h"
#include "insecure_macro.h"
#include "ruse.h" // indirectly adds global + binding

// load secure header
#include "undefine.h"
#define SECURE
#include "secure_macro.h"
#include "ruse.h" // indirectly adds gloval + bindin

#endif

#endif

/*********************************************************************
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

#ifndef EOS_SAFERMEMORYALLOCATION_H
#define EOS_SAFERMEMORYALLOCATION_H

#include "eos_SaferMemoryAllocation.proto.h"

/* redefine malloc as safe_malloc */
#define malloc(n) safe_malloc((size_t) n, (size_t) 1)

/* redefine calloc as safe_calloc */
/* NOTE: This redefinition of calloc must be performed *after* the definition of safe_calloc,
 *       because of the potential of circular macro dependencies. */
#define calloc(n, s) safe_calloc((size_t) n, (size_t) s)

/* redefine realloc as safe_realloc */
#define realloc(p, n) safe_realloc((void*) p, (size_t) n, (size_t) 1)

/* redefine free */
//#define EOS_FREE(p) { assert(p != NULL); free(p); p=NULL; }
#define EOS_FREE(p) {if(p != NULL) free(p); p=NULL;}

#endif

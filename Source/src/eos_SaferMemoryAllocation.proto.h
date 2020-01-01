/*********************************************************************
 *
 * Filetype: (HEADER)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/
#ifndef EOS_SAFERMEMORYALLOCATION_PROTOTYPES

#define EOS_SAFERMEMORYALLOCATION_PROTOTYPES

/*
 * ------------------------------------------------------------
 * 2 PUBLIC FUNCTION PROTOTYPES FOR eos_SaferMemoryAllocation.c
 * ------------------------------------------------------------
 */

void* safe_malloc(size_t n, size_t size);
void* safe_calloc(size_t n, size_t size);
void* safe_realloc(void* p, size_t n, size_t size);

#ifdef _EOS_SAFERMEMORYALLOCATION_INTERNAL_PROTOTYPES

/*
 * -------------------------------------------------------------
 * 0 PRIVATE FUNCTION PROTOTYPES FOR eos_SaferMemoryAllocation.c
 * -------------------------------------------------------------
 */

#endif /* _EOS_SAFERMEMORYALLOCATION_INTERNAL_PROTOTYPES */

#endif /* EOS_SAFERMEMORYALLOCATION_PROTOTYPES */

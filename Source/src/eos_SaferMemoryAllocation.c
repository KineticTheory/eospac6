/*! ******************************************************************
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

//#define EOS_PAD_MEMORY_
#ifdef EOS_PAD_MEMORY_
   /* this is used for debugging */
#  define PAD_SIZE(m) (m * 2)
#else
#  define PAD_SIZE(m) (m)
#endif

/***********************************************************************/
/*! \brief Safer malloc
 ***********************************************************************/
void*
safe_malloc(size_t n, size_t size)
{
  size_t m = (size_t)PAD_SIZE(n);
  void *p = NULL;
  assert (p == NULL);

  p = (void*)calloc(m, size);
  if (p == NULL) {
    printf("safe_malloc failed to allocate %ld bytes\n", (size_t)(m * size));
    assert(p != NULL);
  }
  return p;
}

/***********************************************************************/
/*! \brief Safer calloc
 ***********************************************************************/
void*
safe_calloc(size_t n, size_t size)
{
  size_t m = (size_t)PAD_SIZE(n);
  void *p = NULL;
  assert (p == NULL);

  p = (void*)calloc(m, size);
  if (p == NULL) {
    printf("safe_calloc failed to allocate %ld bytes\n", (size_t)(m * size));
    assert(p != NULL);
  }
  return p;
}

/***********************************************************************/
/*! \brief Safer realloc
 ***********************************************************************/
void*
safe_realloc(void* p, size_t n, size_t size)
{
  size_t m = (size_t)PAD_SIZE(n);
  void *ptr = NULL;

  if (m <= 0) /* retain current value of p */
    return NULL;

  if (p == NULL) {
    ptr = safe_malloc(m, size);
  }
  else {
    ptr = realloc(p, m * size);
  }

  if (ptr == NULL) {
    printf("safe_realloc failed to allocate %ld bytes\n", (size_t)(m * size));
    assert(ptr != NULL);
  }

  return ptr;
}

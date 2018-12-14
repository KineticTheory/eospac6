

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
int ses_read_next_(ses_file_handle* pt_handle, ses_word return_value2[], int* pdim1) {
#else

#ifdef UC_UNDER
  int SES_READ_NEXT_(ses_file_handle* pt_handle, ses_word return_value2[], int* pdim1) {
#else

#ifdef LC_NOUNDER
    int ses_read_next(ses_file_handle* pt_handle, ses_word return_value2[], int* pdim1) {
#else

#ifdef UC_NOUNDER
      int SES_READ_NEXT(ses_file_handle* pt_handle, ses_word return_value2[], int* pdim1) {
#endif

#endif
#endif
#endif

  ses_word_reference return_value = (ses_word_reference)NULL;

  ses_file_handle the_handle = *pt_handle;
  int dim1 = *pdim1;
#ifdef DEBUG_WRAP
  printf("wrap_ses_read_next.c:  the_handle is %d\n", the_handle);
  printf("wrap_ses_read_next.c:  dim1 is %d\n", dim1);
#endif

  return_value = ses_read_next(the_handle);

  int i = 0;
  for (i=0; i < dim1; i++) {
    return_value2[i] = return_value[i];
  }
#ifdef DEBUG_WRAP 
  printf("wrap_ses_read_next.c:  returning\n");
  printf("wrap_ses_read_next.c:  return_value is %e %e %e\n", return_value[0], return_value[1], return_value[2]);
  printf("wrap_ses_read_next.c:  return_value2 is %e %e %e\n", return_value2[0], return_value2[1], return_value2[2]);
#endif
  return 0;

}

 

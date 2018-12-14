

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_error_flag ses_change_next_(ses_file_handle* pt_handle, ses_word_reference pt_buffer, ses_number* pt_dim1) {
#else

#ifdef UC_UNDER
ses_error_flag SES_CHANGE_NEXT_(ses_file_handle* pt_handle, ses_word_reference pt_buffer, ses_number* pt_dim1) {
#else

#ifdef LC_NOUNDER
ses_error_flag ses_change_next(ses_file_handle* pt_handle, ses_word_reference pt_buffer, ses_number* pt_dim1) {
#else

#ifdef UC_NOUNDER
ses_error_flag SES_CHANGE_NEXT(ses_file_handle* pt_handle, ses_word_reference pt_buffer, ses_number* pt_dim1) {
#endif

#endif
#endif
#endif

  ses_error_flag return_value;

  ses_file_handle the_handle = *pt_handle;
  ses_number dim1 = *pt_dim1;

#ifdef DEBUG_WRAP
  printf("wrap_ses_change_next.c:  the_handle is %d\n", the_handle);
  printf("wrap_ses_change_next.c:  what's in the_buffer is %e\n", pt_buffer[0]);
  printf("wrap_ses_change_next.c:  dim1 is %d\n", dim1);
#endif

  return_value = ses_change_next(the_handle, pt_buffer, dim1);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_change_next.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 

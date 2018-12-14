

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_error_flag ses_write_next_(ses_file_handle* pt_handle, ses_word_reference buffer, ses_number* pt_dim1, ses_label the_label) {
#else

#ifdef UC_UNDER
ses_error_flag SES_WRITE_NEXT_(ses_file_handle* pt_handle, ses_word_reference buffer, ses_number* pt_dim1, ses_label the_label) {
#else

#ifdef LC_NOUNDER
ses_error_flag ses_write_next(ses_file_handle* pt_handle, ses_word_reference buffer, ses_number* pt_dim1, ses_label the_label) {
#else

#ifdef UC_NOUNDER
ses_error_flag SES_WRITE_NEXT(ses_file_handle* pt_handle, ses_word_reference buffer, ses_number* pt_dim1, ses_label the_label) {
#endif

#endif
#endif
#endif

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle the_handle = *pt_handle;
  ses_number dim1 = *pt_dim1;
#ifdef DEBUG_WRAP
  printf("wrap_ses_write_next.c:  the_handle is %d\n", the_handle);
  printf("wrap_ses_write_next.c:  buf[0] is %e\n", buffer[0]);
  printf("wrap_ses_write_next.c:  dim1 is %d\n", dim1);
  printf("wrap_ses_write_next.c:  label is %s\n", the_label);
#endif

  return_value = ses_write_next(the_handle, buffer, dim1, the_label);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_write_next.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 

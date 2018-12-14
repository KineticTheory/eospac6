

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_error_flag ses_write_comments_c_(ses_file_handle* pt_handle, ses_string the_string, ses_number* pt_dim1) {
#else

#ifdef UC_UNDER
ses_error_flag SES_WRITE_COMMENTS_C_(ses_file_handle* pt_handle, ses_string the_string, ses_number* pt_dim1) {
#else

#ifdef LC_NOUNDER
ses_error_flag ses_write_comments_c(ses_file_handle* pt_handle, ses_string the_string, ses_number* pt_dim1) {
#else

#ifdef UC_NOUNDER
ses_error_flag SES_WRITE_COMMENTS_C(ses_file_handle* pt_handle, ses_string the_string, ses_number* pt_dim1) {
#endif

#endif
#endif
#endif

  ses_error_flag return_value = SES_WRITE_ERROR;

  ses_file_handle the_handle = *pt_handle;
  ses_number dim1 = *pt_dim1;

#ifdef DEBUG_WRAP
  printf("wrap_ses_write_comments.c:  the_handle is %d\n", the_handle);
  printf("wrap_ses_write_comments.c:  dim1 is %d\n", dim1);
#endif

  return_value = ses_write_comments(the_handle, the_string, dim1);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_write_comments.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 

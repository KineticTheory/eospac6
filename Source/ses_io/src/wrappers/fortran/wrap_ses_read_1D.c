

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_error_flag ses_read_1d_(ses_file_handle* pt_handle, ses_word_reference buffer, ses_number* pt_dim) {
#else

#ifdef UC_UNDER
ses_error_flag SES_READ_1D_(ses_file_handle* pt_handle,  ses_word_reference buffer, ses_number* pt_dim) {
#else

#ifdef LC_NOUNDER
ses_error_flag ses_read_1d(ses_file_handle* pt_handle,  ses_word_reference buffer, ses_number* pt_dim) {
#else

#ifdef UC_NOUNDER
ses_error_flag SES_READ_1D(ses_file_handle* pt_handle, ses_word_reference buffer, ses_number* pt_dim) {
#endif

#endif
#endif
#endif

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle the_handle = *pt_handle;
  ses_number dim = *pt_dim;
#ifdef DEBUG_WRAP
  printf("wrap_ses_read_1D.c:  the_handle is %d\n", the_handle);
  printf("wrap_ses_read_1D.c:  the_dim is %d\n", dim);
#endif

  return_value = ses_read_1D(the_handle, buffer, dim);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_read_1D.c:  return_value is %d\n", return_value);
  printf("wrap_ses_read_1D.c:  buffer[1] is %e\n", buffer[1]);
#endif
  return return_value;

}

 

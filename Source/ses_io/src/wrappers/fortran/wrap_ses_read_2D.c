

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_error_flag ses_read_2d_(ses_file_handle* pt_handle, ses_word_reference buffer, ses_number* pt_dim1, ses_number* pt_dim2) {
#else

#ifdef UC_UNDER
ses_error_flag SES_READ_2D_(ses_file_handle* pt_handle,  ses_word_reference buffer, ses_number* pt_dim1, ses_number* pt_dim2) {
#else

#ifdef LC_NOUNDER
ses_error_flag ses_read_2d(ses_file_handle* pt_handle,  ses_word_reference buffer, ses_number* pt_dim1, ses_number* pt_dim2) {
#else

#ifdef UC_NOUNDER
ses_error_flag SES_READ_2D(ses_file_handle* pt_handle, ses_word_reference buffer, ses_number* pt_dim1, ses_number* pt_dim2) {
#endif

#endif
#endif
#endif

  ses_error_flag return_value = SES_NO_ERROR;


  ses_file_handle the_handle = *pt_handle;
  ses_number dim1 = *pt_dim1;
  ses_number dim2 = *pt_dim2;
#ifdef DEBUG_WRAP
  printf("wrap_ses_read_2D.c:  the_handle is %d\n", the_handle);
  printf("wrap_ses_read_2D.c:  the_dim1 is %d\n", dim1);
  printf("wrap_ses_read_2D.c:  the_dim2 is %d\n", dim2);
#endif

  return_value = ses_read_2D(the_handle, buffer, dim1, dim2);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_read_2D.c:  return_value is %d\n", return_value);
  printf("wrap_ses_read_2D.c:  buffer[0] is %e\n", buffer[0]);
#endif
  return return_value;

}

 

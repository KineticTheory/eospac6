

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_error_flag ses_read_pairs_(ses_file_handle* pt_handle, ses_word_reference buf1, ses_word_reference buf2, ses_number* pt_dim1) {
#else

#ifdef UC_UNDER
ses_error_flag SES_READ_PAIRS_(ses_file_handle* pt_handle, ses_word_reference buf1, ses_word_reference buf2, ses_number* pt_dim1) {
#else

#ifdef LC_NOUNDER
ses_error_flag ses_READ_PAIRS(ses_file_handle* pt_handle, ses_word_reference buf1, ses_word_reference buf2, ses_number* pt_dim1) {
#else

#ifdef UC_NOUNDER
ses_error_flag SES_READ_PAIRS(ses_file_handle* pt_handle, ses_word_reference buf1, ses_word_reference buf2, ses_number* pt_dim1)  {
#endif

#endif
#endif
#endif

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle the_handle = *pt_handle;
  ses_number dim1 = *pt_dim1;
#ifdef DEBUG_WRAP
  printf("wrap_ses_read_pairs.c:  the_handle is %d\n", the_handle);
  printf("wrap_ses_read_pairs.c:  dim1 is %d\n", dim1);
#endif

  return_value = ses_read_pairs(the_handle, buf1, buf2, dim1);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_read_pairs.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 

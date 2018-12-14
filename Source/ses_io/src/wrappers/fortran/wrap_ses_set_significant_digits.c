

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_error_flag ses_set_significant_digits_(ses_file_handle* pt_handle, ses_number* pt_num_digits) {
#else

#ifdef UC_UNDER
ses_error_flag SES_SET_SIGNIFICANT_DIGITS_(ses_file_handle* pt_handle, ses_number* pt_num_digits) {
#else

#ifdef LC_NOUNDER
ses_error_flag ses_set_significant_digits(ses_file_handle* pt_handle, ses_number* pt_num_digits) {
#else

#ifdef UC_NOUNDER
ses_error_flag SES_set_significant_digits(ses_file_handle* pt_handle, ses_number* pt_num_digits) {
#endif

#endif
#endif
#endif

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle the_handle = *pt_handle;
  ses_number num_digits = *pt_num_digits;
#ifdef DEBUG_WRAP
  printf("wrap_ses_set_significant_digits.c:  the_handle is %d\n", the_handle);
  printf("wrap_ses_set_significant_digits.c:  num_digits is %d\n", num_digits);
#endif

  return_value = ses_set_significant_digits(the_handle, num_digits);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_set_significant_digits.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 

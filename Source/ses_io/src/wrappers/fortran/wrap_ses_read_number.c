

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_error_flag ses_read_number_(ses_file_handle* pt_handle, ses_number_reference buffer) {
#else

#ifdef UC_UNDER
ses_error_flag SES_READ_NUMBER_(ses_file_handle* pt_handle, ses_number_reference buffer) {
#else

#ifdef LC_NOUNDER
ses_error_flag ses_read_number(ses_file_handle* pt_handle, ses_number_reference buffer) {
#else

#ifdef UC_NOUNDER
ses_error_flag SES_READ_NUMBER(ses_file_handle* pt_handle, ses_number_reference buffer) {
#endif

#endif
#endif
#endif

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle the_handle = *pt_handle;
#ifdef DEBUG_WRAP
  printf("wrap_ses_read_number.c:  the_handle is %d\n", the_handle);
#endif

  return_value = ses_read_number(the_handle, buffer);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_read_number.c:  return_value is %d\n", return_value);
  printf("wrap_ses_read_number.c:  buffer is %d\n", *buffer);
#endif
  return return_value;

}

 

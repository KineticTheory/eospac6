

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_error_flag ses_combine_(ses_file_handle* handle1, ses_file_handle* handle2, ses_string new_filename) {
#else

#ifdef UC_UNDER
ses_error_flag SES_COMBINE_(ses_file_handle* handle1, ses_file_handle* handle2, ses_string new_filename) {
#else

#ifdef LC_NOUNDER
ses_error_flag ses_combine(ses_file_handle* handle1, ses_file_handle* handle2, ses_string new_filename) {
#else

#ifdef UC_NOUNDER
ses_error_flag SES_COMBINE(ses_file_handle* handle1, ses_file_handle* handle2, ses_string new_filename) {
#endif

#endif
#endif
#endif

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle the_handle1 = *handle1;
  ses_file_handle the_handle2 = *handle2;
#ifdef DEBUG_WRAP
  printf("wrap_ses_combine.c:  the_handle1 is %d\n", the_handle1);
  printf("wrap_ses_combine.c:  the_handle2 is %d\n", the_handle2);
  printf("wrap_ses_combine.c:  new_filename is %s\n", new_filename);
#endif

  return_value = ses_combine(the_handle1, the_handle2, new_filename);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_combine.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 

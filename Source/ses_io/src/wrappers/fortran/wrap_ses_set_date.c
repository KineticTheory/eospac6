

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#include <string.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
  ses_error_flag ses_set_date_(ses_file_handle* pt_handle, long* the_date) {
#else

#ifdef UC_UNDER
  ses_error_flag SES_SET_DATE_(ses_file_handle* pt_handle, long* the_date) {
#else

#ifdef LC_NOUNDER
  ses_error_flag ses_set_date(ses_file_handle* pt_handle, long* the_date) {
#else

#ifdef UC_NOUNDER
  ses_error_flag SES_SET_DATE(ses_file_handle* pt_handle, long* the_date) {
#endif

#endif
#endif
#endif

  ses_file_handle the_handle = *pt_handle;
  long myDate = *the_date;
#ifdef DEBUG_WRAP
  printf("wrap_ses_set_date.c:  the_handle is %d\n", the_handle);
  printf("wrap_ses_set_date.c:  the_date is %d\n", the_date);
#endif

  ses_error_flag return_value = ses_set_date(the_handle, myDate);
  
 
#ifdef DEBUG_WRAP
  printf("wrap_ses_date.c:  returning error flag = %d\n", return_value);
#endif
 
  return return_value;
}

 

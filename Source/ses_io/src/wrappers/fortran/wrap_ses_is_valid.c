

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_boolean ses_is_valid_(ses_file_handle* pt_handle) {
#else

#ifdef UC_UNDER
ses_boolean SES_IS_VALID_(ses_file_handle* pt_handle) {
#else

#ifdef LC_NOUNDER
ses_boolean ses_is_valid(ses_file_handle* pt_handle) {
#else

#ifdef UC_NOUNDER
ses_boolean SES_IS_VALID(ses_file_handle* pt_handle) {
#endif

#endif
#endif
#endif

  ses_boolean return_value = SES_NO_ERROR;

  ses_file_handle the_handle = *pt_handle;
#ifdef DEBUG_WRAP
  printf("wrap_ses_is_valid.c:  the_handle is %d\n", the_handle);
#endif

  return_value = ses_is_valid(the_handle);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_is_valid.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 

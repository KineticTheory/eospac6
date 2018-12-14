

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_boolean ses_indicates_error_(ses_error_flag* pt_error) {
#else

#ifdef UC_UNDER
ses_boolean SES_indicates_error_(ses_error_flag* pt_error) {
#else

#ifdef LC_NOUNDER
ses_boolean ses_indicates_error(ses_error_flag* pt_error) {
#else

#ifdef UC_NOUNDER
ses_boolean SES_indicates_error(ses_error_flag* pt_error) {
#endif

#endif
#endif
#endif

  ses_boolean return_value = SES_FALSE;

  ses_error_flag the_error = *pt_error;
#ifdef DEBUG_WRAP
  printf("wrap_ses_indicates_error.c:  the_error is %d\n", the_error);
#endif

  return_value = ses_indicates_error(the_error);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_indicates_error.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 

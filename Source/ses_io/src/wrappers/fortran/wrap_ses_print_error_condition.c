

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
int ses_print_error_condition_(ses_file_handle* pt_handle, ses_string the_string) {
#else

#ifdef UC_UNDER
int SES_PRINT_ERROR_CONDITION_(ses_file_handle* pt_handle, ses_string the_string {
#else

#ifdef LC_NOUNDER
int ses_print_error_condition(ses_file_handle* pt_handle, ses_string the_string {
#else

#ifdef UC_NOUNDER
int SES_PRINT_ERROR_CONDITION(ses_file_handle* pt_handle, ses_string the_string) {
#endif

#endif
#endif
#endif

 
  ses_file_handle the_handle = *pt_handle;
#ifdef DEBUG_WRAP
  printf("wrap_ses_print_error_condition.c:  the_handle is %d\n", the_handle);
#endif

  ses_string almost_the_string = ses_print_error_condition(the_handle);
  if (almost_the_string != (ses_string)NULL) {
    strcpy(the_string, almost_the_string);
  }
  else {
    the_string = (ses_string)NULL;
  }

#ifdef DEBUG_WRAP 
  printf("wrap_ses_print_error_condition.c:  returning string is %s\n", the_string);
  printf("wrap_ses_print_error_condition.c:  returning %d\n", strlen(the_string));
#endif
  return strlen(the_string);

}

 

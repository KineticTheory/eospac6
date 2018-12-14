

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#define DEBUG_WRAP

#ifdef LC_UNDER
ses_error_flag ses_set_label_(ses_file_handle* pt_handle, ses_label the_label) {
#else

#ifdef UC_UNDER
ses_error_flag SES_SET_LABEL_(ses_file_handle* pt_handle, ses_label the_label) {
#else

#ifdef LC_NOUNDER
ses_error_flag ses_set_label(ses_file_handle* pt_handle, ses_label the_label) {
#else

#ifdef UC_NOUNDER
ses_error_flag SES_SET_LABEL(ses_file_handle* pt_handle, ses_label the_label) {
#endif

#endif
#endif
#endif

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle the_handle = *pt_handle;
  the_label[60-1] = '\0';
#ifdef DEBUG_WRAP
  printf("wrap_ses_set_label.c:  the_handle is %d\n", the_handle);
  printf("wrap_ses_set_label.c:  the_label is %s\n", the_label);
#endif

  return_value = ses_set_label(the_handle, the_label);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_set_label.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 

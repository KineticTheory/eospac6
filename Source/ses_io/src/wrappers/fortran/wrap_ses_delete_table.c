

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_error_flag ses_delete_table_(ses_file_handle* pt_handle) {
#else

#ifdef UC_UNDER
ses_error_flag SES_DELETE_TABLE_(ses_file_handle* pt_handle) {
#else

#ifdef LC_NOUNDER
ses_error_flag ses_delete_table(ses_file_handle* pt_handle) {
#else

#ifdef UC_NOUNDER
ses_error_flag SES_DELETE_TABLE(ses_file_handle* pt_handle) {
#endif

#endif
#endif
#endif

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle the_handle = *pt_handle;
#ifdef DEBUG_WRAP
  printf("wrap_ses_delete_table.c:  the_handle is %d\n", the_handle);
#endif

  return_value = ses_delete_table(the_handle);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_delete_table.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 



#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
  ses_error_flag ses_set_table_index_(ses_file_handle* pt_handle, long date1, long date2, long version1) {
#else

#ifdef UC_UNDER
  ses_error_flag SES_SET_TABLE_INDEX_(ses_file_handle* pt_handle, long date1, long date2, long version1) {
#else

#ifdef LC_NOUNDER
  ses_error_flag ses_set_table_index(ses_file_handle* pt_handle, long date1, long date2, long version1) {
#else

#ifdef UC_NOUNDER
  ses_error_flag SES_SET_TABLE_INDEX(ses_file_handle* pt_handle, long date1, long date2, long version1) {
#endif

#endif
#endif
#endif

  ses_error_flag return_value = SES_NO_ERROR;
  
  ses_file_handle the_handle = *pt_handle;

  long date11 = 0;
  long date12 = 0;
  long version = 0;
  
  return_value = ses_set_table_index(the_handle, date11, date12, version);

  return return_value;

}

 

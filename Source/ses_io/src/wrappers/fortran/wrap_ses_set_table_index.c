

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
  ses_error_flag ses_set_table_index_(ses_file_handle* pt_handle, long *ptr_date1, long *ptr_date2, long *ptr_version1) {
#else

#ifdef UC_UNDER
  ses_error_flag SES_SET_TABLE_INDEX_(ses_file_handle* pt_handle, long *ptr_date1, long *ptr_date2, long *ptr_version1) {
#else

#ifdef LC_NOUNDER
  ses_error_flag ses_set_table_index(ses_file_handle* pt_handle, long *ptr_date1, long *ptr_date2, long *ptr_version1) {
#else

#ifdef UC_NOUNDER
  ses_error_flag SES_SET_TABLE_INDEX(ses_file_handle* pt_handle, long *ptr_date1, long *ptr_date2, long *ptr_version1) {
#endif

#endif
#endif
#endif

  ses_error_flag return_value = SES_NO_ERROR;
  
  ses_file_handle the_handle = *pt_handle;
  
  long date1 = *ptr_date1;
  long date2 = *ptr_date2;
  long version = *ptr_version1;

#ifdef DEBUG_WRAP
  printf("WRAPPER ses_set_table_index: handle: %d date1: %ld, date2: %ld version1 %ld\n", the_handle, date1, date2, version);
#endif  

  return_value = ses_set_table_index(the_handle, date1, date2, version);

#ifdef DEBUG_WRAP
  printf("WRAPPER ses_set_table_index: return_value: %d\n", return_value);
#endif
  return return_value;

}

 

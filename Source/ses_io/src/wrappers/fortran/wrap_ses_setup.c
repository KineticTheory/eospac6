

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_error_flag ses_setup_(ses_file_handle* pt_handle, ses_material_id* pt_mid, ses_table_id* pt_tid) {
#else

#ifdef UC_UNDER
ses_error_flag SES_SETUP_(ses_file_handle* pt_handle, ses_material_id* pt_mid, ses_table_id* pt_tid) {
#else

#ifdef LC_NOUNDER
ses_error_flag ses_setup(ses_file_handle* pt_handle, ses_material_id* pt_mid, ses_table_id* pt_tid) {
#else

#ifdef UC_NOUNDER
ses_error_flag SES_SETUP(ses_file_handle* pt_handle, ses_material_id* pt_mid, ses_table_id* pt_tid) {
#endif

#endif
#endif
#endif

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle the_handle = *pt_handle;
  ses_material_id mid = *pt_mid;
  ses_table_id tid = *pt_tid;
#ifdef DEBUG_WRAP
  printf("wrap_ses_setup.c:  the_handle is %d\n", the_handle);
  printf("wrap_ses_setup.c:  the_mid is %d\n", mid);
  printf("wrap_ses_setup.c:  the_tid is %d\n", tid);
#endif

  return_value = ses_setup(the_handle, mid, tid);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_setup.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 

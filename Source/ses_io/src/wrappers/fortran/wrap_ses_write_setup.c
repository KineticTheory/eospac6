
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_error_flag ses_write_setup_(ses_file_handle* pt_handle, ses_material_id* pt_mid, ses_table_id* pt_tid,  ses_number nr, ses_number nt, ses_number ntab) {
#else

#ifdef UC_UNDER
ses_error_flag SES_WRITE_SETUP_(ses_file_handle* pt_handle, ses_material_id* pt_mid, ses_table_id* pt_tid,  ses_number nr, ses_number nt, ses_number ntab) {
#else

#ifdef LC_NOUNDER
ses_error_flag ses_WRITE)setup(ses_file_handle* pt_handle, ses_material_id* pt_mid, ses_table_id* pt_tid,  ses_number nr, ses_number nt, ses_number ntab) {
#else

#ifdef UC_NOUNDER
ses_error_flag SES_WRITE_SETUP(ses_file_handle* pt_handle, ses_material_id* pt_mid, ses_table_id* pt_tid,  ses_number nr, ses_number nt, ses_number ntab) {
#endif

#endif
#endif
#endif

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle the_handle = *pt_handle;
  ses_material_id mid = *pt_mid;
  ses_table_id tid = *pt_tid;

  return_value = ses_write_setup(the_handle, mid, tid, nr, nt, ntab);
  return return_value;

}

 

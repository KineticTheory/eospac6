#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_boolean ses_get_grid_(ses_file_handle* pt_handle, ses_material_id* the_mid, ses_table_id* the_tid, long* nr, long* nt, long* ntab) {
#else

#ifdef UC_UNDER
ses_boolean SES_GET_GRID_(ses_file_handle* pt_handle, ses_material_id the_mid, ses_table_id the_tid, long* nr, long* nt, long* ntab) {
#else

#ifdef LC_NOUNDER
ses_boolean ses_get_grid(ses_file_handle* pt_handle, ses_material_id the_mid, ses_table_id the_tid, long* nr, long* nt, long* ntab) {
#else

#ifdef UC_NOUNDER
ses_boolean SES_GET_GRID(ses_file_handle* pt_handle, ses_material_id the_mid, ses_table_id the_tid, long* nr, long* nt, long* ntab) {
#endif

#endif
#endif
#endif

  ses_file_handle the_handle = *pt_handle;
  ses_material_id my_mid = *the_mid;
  ses_table_id my_tid = *the_tid;

#ifdef DEBUG_WRAP
  printf("wrap_ses_get_grid.c:  the_mid is %d the_tid is %d\n", my_mid, my_tid);
  printf("wrap_ses_get_grid.c:  calling ses_get_grid *nr is %ld *nt is %ld\n", *nr, *nt);
#endif

  ses_boolean return_value = ses_get_grid(the_handle, my_mid, my_tid, nr, nt, ntab);

  return return_value;

}


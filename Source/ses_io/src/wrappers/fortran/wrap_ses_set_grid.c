

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_error_flag ses_set_grid_(ses_file_handle* pt_handle, ses_number* pt_nr, ses_number* pt_nt, ses_number* pt_ntab) {
#else

#ifdef UC_UNDER
  ses_error_flag SES_SET_GRID_(ses_file_handle* pt_handle, ses_number* pt_nr, ses_number* pt_nt, ses_number* pt_ntab) {
#else

#ifdef LC_NOUNDER
    ses_error_flag ses_set_grid(ses_file_handle* pt_handle, ses_number* pt_nr, ses_number* pt_nt, ses_number* pt_ntab) {
#else

#ifdef UC_NOUNDER
      ses_error_flag SES_SET_GRID(ses_file_handle* pt_handle, ses_number* pt_nr, ses_number* pt_nt, ses_number* pt_ntab) {
#endif

#endif
#endif
#endif

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle the_handle = *pt_handle;
  ses_number nr = *pt_nr;
  ses_number nt = *pt_nt;
  ses_number ntab = *pt_ntab;
#ifdef DEBUG_WRAP
  printf("wrap_ses_set_grid.c:  the_handle is %d\n", the_handle);
  printf("wrap_ses_set_grid.c:  nr is %d\n", nr);
  printf("wrap_ses_set_grid.c:  nt is %d\n", nt);
  printf("wrap_ses_set_grid.c:  ntab is %d\n", ntab);
#endif

  return_value = ses_set_grid(the_handle, nr, nt, ntab);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_set_grid.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 

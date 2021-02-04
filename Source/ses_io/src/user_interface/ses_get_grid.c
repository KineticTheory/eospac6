
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_boolean ses_get_grid(ses_file_handle the_handle, ses_material_id the_mid, 
			 ses_table_id the_tid, long* nr, long* nt, long* ntab) {

  /* FILE* pFILE = 0; */
  /* pFILE = */ _getPFILE(FILE_LIST[the_handle]->_the_handle);
 
  ses_boolean rv = _get_grid(the_handle, the_mid, the_tid, nr, nt, ntab);
#ifdef DEBUG_SES_GET_GRID
    printf("ses_get_grid:  rv: %d\n", rv);
    printf("ses_get_grid:  *nr is %ld and *nt is %ld and *ntab is %ld\n", *nr, *nt, *ntab);
#endif
  return rv;
}

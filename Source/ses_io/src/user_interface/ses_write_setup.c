

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"


ses_error_flag ses_write_setup(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid, ses_number nr, ses_number nt, ses_number ntab) {


	ses_error_flag return_value = SES_NO_ERROR;



	return_value = ses_set_grid(the_handle, nr, nt, ntab);
	if (return_value == SES_NO_ERROR) {
		return_value = ses_setup(the_handle, the_mid, the_tid);
        }


	return return_value;
}









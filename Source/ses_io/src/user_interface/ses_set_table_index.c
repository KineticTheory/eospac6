
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_boolean ses_set_table_index(ses_file_handle the_handle,        
		 long date1, long date2, long version) {

  ses_boolean return_value = SES_TRUE;

  /*  This routine sets the dates and versions in the currently setup table index */

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_set_table_index:  file handle not valid\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_FALSE;
  }


  struct _ses_setup* pSET = FILE_LIST[the_handle]->_the_setup;
  if (pSET == (struct _ses_setup*)NULL) {

#ifdef DEBUG_PRINT
    printf("ses_set_table_index:  null setup \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

    /*  at this point, we're ready to go */

  pSET->_date1 = date1;
  pSET->_date2 = date2;
  pSET->_vers = version;


  return return_value;
}


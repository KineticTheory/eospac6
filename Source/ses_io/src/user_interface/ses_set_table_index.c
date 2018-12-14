
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#define DEBUG_SES_SET_TABLE_INDEX


ses_boolean ses_set_table_index(ses_file_handle the_handle,        
		 long date1, long date2, long version) {

  ses_boolean return_value = SES_NO_ERROR;

  /*  This routine sets the dates and versions in the currently setup table index */

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_set_table_index:  file handle not valid\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_INVALID_FILE_HANDLE;
  }


  struct _ses_setup* pSET = FILE_LIST[the_handle]->_the_setup;
  if (pSET == (struct _ses_setup*)NULL) {

#ifdef DEBUG_PRINT
    printf("ses_set_table_index:  null setup \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

    /*  at this point, we're ready to go */

  pSET->_date1 = date1;
  pSET->_date2 = date2;
  pSET->_vers = version;

#ifdef DEBUG_SES_SET_TABLE_INDEX

  struct _ses_data_record* pDR = FILE_LIST[the_handle]->_current_data_record;
  if (pDR != (struct _ses_data_record*)NULL) {
	pDR->_date1 = date1;
	pDR->_date2 = date2;
	pDR->_vers = version;
  }

  struct _ses_index_record* pIR = FILE_LIST[the_handle]->_current_index_record;
  if (pIR != (struct _ses_index_record*)NULL) {
	pIR->_date1 = date1;
	pIR->_date2 = date2;
	pIR->_vers = version;
  }

#endif


  return return_value;
}



#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


long ses_access_table_index(ses_file_handle the_handle,        
		 ses_table_id_reference* return_tblid, long** nwds, long** iadr,
		 long* date1, long* date2, long* version) {

  long return_value = 0;

  /*  This routine gets all information in the index record at one time */

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_access_table_index:  file handle not valid\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return 0;
  }


  /*  if the arrays passed in are NOT null, error */

  if (*return_tblid != (ses_table_id*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_access_table_index:  tblid pointer NOT NULL\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return 0;
     
  }
  
  

  if (*nwds != (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_access_table_index:  nwds pointer NOT NULL\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  if (*iadr != (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_access_table_index:  iadr pointer NOT NULL\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  /*  if the current table index */

  struct _ses_index_record* ptIR = FILE_LIST[the_handle]->_current_index_record;
  if (ptIR == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_access_table_index:  null index record \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }


  /*  at this point, we're ready to go */


  date1[0] = ptIR->_date1;
  date2[0] = ptIR->_date2;
  version[0] = ptIR->_vers;


  if (date1[0] == 0) {
	  struct _ses_data_record* ptDR = FILE_LIST[the_handle]->_current_data_record;
	  date1[0] = ptDR->_date1;
	  date2[0] = ptDR->_date2;
	  version[0] = ptDR->_vers;
  }

  ses_material_id the_mid = 0;
  the_mid = ptIR->_mid;

  long size = 0;

  return_tblid[0] = (ses_table_id_reference)NULL;
  return_tblid[0] = ses_get_table_ids(the_handle, the_mid, &size);
  nwds[0] = _get_nwds_index_record(ptIR);
  iadr[0] = _get_iadr_index_record(ptIR);
  return_value = size;

  return return_value;
}


#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


long _get_address_for_table_ascii(struct _ses_index_record* ptIR, ses_table_id the_tid, struct _ses_file_handle* pSFH) {

  /*  get the 'file' address for the associated table id */
   
  /*********************************************************************/
  /*  error check the arguments */
  
  if (ptIR == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_address_for_table: _get_address_for_table passed a NULL record\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  if (_is_valid_tid(the_tid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_address_for_table: _get_address_for_table passed an invalid tid\n");
#endif
    _set_latest_error(SES_INVALID_TID);
    return 0;
  }

  if (ptIR->_nrec <= 0) {
#ifdef DEBUG_PRINT
    printf("_get_address_for_table: nrec <= 0 in _get_address_for_table\n");
#endif
    _set_latest_error(SES_INVALID_TID);
    return 0;
  }
  /*********************************************************************/

  /*  find the table id and return */

  long return_value = 0;
  
  int i=0;
  int num_markers = 0;
  ses_boolean seen_marker = SES_FALSE;
  for (i=0; i<ptIR->_nrec; i++) {

    if (seen_marker == SES_FALSE) {
	num_markers++;
    }
    if (the_tid == ptIR->_tblid[i]){
       return_value = ptIR->_iadr[i];
       seen_marker = SES_TRUE;
    }
    

  }

  
  return_value = return_value;  //  one marker for the index record
  
  return return_value;
}



#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

ses_error_flag ses_delete_table(ses_file_handle the_handle) {

  ses_error_flag return_value = SES_NO_ERROR;

  /*  need to destruct current index record
      need to destruct current data record
      need to destruct current data arrrays (in the current data record) */

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_delete_table:  invalid file handle\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_INVALID_FILE_HANDLE;
  }

  struct _ses_setup* pSET = FILE_LIST[the_handle]->_the_setup;
  if (pSET->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_delete_table:  setup incomplete\n");
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_SETUP_ERROR;
  }
  else {
#ifdef DEBUG_PRINT
    printf("ses_delete_table:  setup complete\n");
#endif
  }

  ses_material_id the_mid = pSET->_mid;
  ses_table_id the_tid = pSET->_tid;

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  /* FILE* pFILE = 0; */
  /* pFILE = */  _getPFILE(pSFH);
 
  struct _ses_index_record* ptIR = 0;
  ptIR = FILE_LIST[the_handle]->_current_index_record;

  struct _ses_data_record* ptDR = 0;
  ptDR = FILE_LIST[the_handle]->_current_data_record;

  /*  copy the index record into a new index record to write over the current record at the right spot */

  struct _ses_index_record* the_copy = _copy_index_record(ptIR);
  if (the_copy == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_delete_table:  copy index record failed\n");
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return SES_FUNCTION_FAIL_ERROR;
  }

  /*  change the_copy so that the table is removed */

  ses_boolean didit_remove = _remove_table_from_index_record(the_copy, ptDR, the_tid);
  if (didit_remove == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_delete_table:  _remove_table_from_index_record failed\n");
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return SES_FUNCTION_FAIL_ERROR;
  }


  /*  go to the right place in the file */

  ses_boolean didit_go = pSFH->pt2_go_to_index_record(the_handle, the_mid, the_tid);
  if (didit_go == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_delete_table:  _go_to_index_record failed\n");
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return SES_FUNCTION_FAIL_ERROR;
  }


  /*  write the new index record on the file at the current position */

  ses_boolean didit_write = _write_index_record(the_copy, pSFH);
  if (didit_write == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_delete_table:  _write_index_record failed\n");
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return SES_FUNCTION_FAIL_ERROR;
  }

  /*  remove the objects */

  _destruct_ses_index_record(ptIR);
  free(ptIR);
  ptIR = (struct _ses_index_record*)NULL;

  FILE_LIST[the_handle]->_current_index_record = (struct _ses_index_record*)NULL;
  FILE_LIST[the_handle]->_current_index_record = the_copy;

  /*  free the data from the removed table in the current data record */

  

  _releasePFILE(pSFH);

  return return_value;
}

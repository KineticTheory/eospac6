
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_error_flag ses_set_grid(ses_file_handle the_handle, ses_number nr, ses_number nt, ses_number ntab) {

  ses_error_flag return_value = SES_NO_ERROR;


  /*  when user passes in a straight -1, that does not cast correctly to a -1 long -- check for this and 
      correct */

  if (ntab > 10000) {
    ntab = (long)(-1);
  }

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_set_grid::  invalid file handle\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_INVALID_FILE_HANDLE;
  }

  if (nr <= 0) {
#ifdef DEBUG_PRINT
    printf("ses_set_grid: nr <= 0\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }

  if (nt <= 0) {
#ifdef DEBUG_PRINT
    printf("ses_set_grid: nt <= 0\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }

  if ((FILE_LIST[the_handle]->_the_handle->_the_open_mode != 'W') &&
      (FILE_LIST[the_handle]->_the_handle->_the_open_mode != 'A')) {
#ifdef DEBUG_PRINT
    printf("ses_set_grid:  open mode is not WRITE or APPEND -- cannot set grid -- open_mode is %c\n",FILE_LIST[the_handle]->_the_handle->_the_open_mode);
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_SETUP_ERROR;
  }

  if (FILE_LIST[the_handle]->_the_setup == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_set_grid: the_setup NULL\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }


  ses_table_id tid = FILE_LIST[the_handle]->_the_setup->_tid;
  if (FILE_LIST[the_handle]->_current_index_record != (struct _ses_index_record*)NULL) {
  	int index = _get_table_index(FILE_LIST[the_handle]->_current_index_record, tid);
  	FILE_LIST[the_handle]->_current_index_record->_nr[index] = nr;
  	FILE_LIST[the_handle]->_current_index_record->_nt[index] = nt;
  }

  FILE_LIST[the_handle]->_the_setup->_nr = nr;
  FILE_LIST[the_handle]->_the_setup->_nt = nt;
  FILE_LIST[the_handle]->_the_setup->_ntab = ntab;

 
  return return_value;

}


#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"



ses_error_flag ses_read_1D(ses_file_handle the_handle, ses_word_reference the_buffer, ses_number dim) {

  /*  at the current file location, read a 1D array */

  ses_error_flag return_value = SES_NO_ERROR;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_read_1D: Invalid file handle in ses_read_1D\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_INVALID_FILE_HANDLE;
  }

  if (FILE_LIST[the_handle]->_the_setup->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_read_1D:  setup not complete \n");
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_SETUP_ERROR;
  }

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_1D: Pointer to ses file handle null in ses_read_1D\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  /* FILE* pFILE = 0; */
  /* pFILE = */ _getPFILE(pSFH);

 
  struct _ses_setup* pSET = FILE_LIST[the_handle]->_the_setup;
  if (_isit_setup(pSET) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_read_1D:  not setup in ses_read_1D\n");
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_SETUP_ERROR;
  }

  if (the_buffer == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_1D: null buffer passed into ses_read_1D\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  if (dim <= 0) {
#ifdef DEBUG_PRINT
    printf("ses_read_1D: dim less than or equal to 0 in ses_read_1d\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  /*  here, we're good to go */

  unsigned int nsig = FILE_LIST[the_handle]->_the_setup->_significant_digits;
  ses_boolean do_valid = FILE_LIST[the_handle]->_the_setup->_do_validation;

 
 if (pSFH->pt2_read_array == NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_1D:  function pointer null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  struct _ses_directory* ptDIR = FILE_LIST[the_handle]->_directory;
  if (ptDIR->_has_multiple_files == SES_TRUE) {
     ses_table_id tid = FILE_LIST[the_handle]->_the_setup->_tid;
     int table_index = _get_table_index(FILE_LIST[the_handle]->_current_index_record, tid);
     pSFH->_array_address = FILE_LIST[the_handle]->_current_index_record->_array_iadr[table_index][pSFH->_iteration_index];
     pSFH->_array_filename = FILE_LIST[the_handle]->_current_index_record->_array_filename[table_index][pSFH->_iteration_index];
     long location = pSFH->_array_address;
     /* ses_boolean didit_go = SES_FALSE; */
     /* didit_go = */ pSFH->pt2_go_to_next_array_location(pSFH, location);
  }
 ses_error_flag didit_read = pSFH->pt2_read_array(pSFH, the_buffer, dim, nsig, do_valid);

  if (didit_read != SES_NO_ERROR) {
#ifdef DEBUG_PRINT
    printf("ses_read_1D:  _read_array error\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  /*  code added to move the iterator along -- found while testing wrappers */
  
  struct _ses_iterator* pIT = FILE_LIST[the_handle]->_current_data_record->_the_iterator;

  if (pIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_number:  iterator null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }
  
  pIT->_current_array++;

  _releasePFILE(pSFH);


  return return_value;
}

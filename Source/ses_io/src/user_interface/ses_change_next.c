

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdlib.h"

ses_error_flag ses_change_next(ses_file_handle the_handle, ses_word_reference the_buffer, ses_number dim) {

  /*  change the next data */

  ses_error_flag return_value = SES_NO_ERROR;


  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_change_next: invalid ses file handle in ses_change_next\n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }

  struct _ses_setup* pSET = FILE_LIST[the_handle]->_the_setup;
  if (pSET->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_change_next: setup not complete \n");
#endif
    return SES_SETUP_ERROR;
  }

  if (the_buffer == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_change_next: the buffer is null \n");
#endif
    return SES_NULL_OBJECT_ERROR;
  }

  if (dim <= 0) {
#ifdef DEBUG_PRINT
    printf("ses_change_next:  buffer size <= 0\n");
#endif
    return SES_OBJECT_OUT_OF_RANGE;
  }

  /*  check the dimension given for the buffer against the expected size */

  ses_number expected_size = ses_array_size_next(the_handle);
  if (expected_size != (ses_number)dim) {
#ifdef DEBUG_PRINT
    printf("ses_change_next:  dim %ld not expected size %ld\n", (long)dim, (long)expected_size);
#endif
    return SES_OBJECT_OUT_OF_RANGE;
  }
  
  /*  change the data */


  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  if (pSFH == (struct _ses_file_handle*)NULL) {
    return SES_NULL_OBJECT_ERROR;
  }

  FILE* pFILE = _getPFILE(pSFH);
  if (pFILE == (FILE*)NULL) {
    return SES_NULL_OBJECT_ERROR;
  }
  

  ses_boolean didit_write =  SES_FALSE;
  didit_write = _write_ses_word_array(pSFH, the_buffer, dim, 0, SES_FALSE);

  struct _ses_iterator* pIT = FILE_LIST[the_handle]->_current_data_record->_the_iterator;
  pIT->_current_array++;

  _releasePFILE(pSFH);

  return return_value;
}


#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"


ses_error_flag ses_read_word(ses_file_handle the_handle, ses_word_reference the_buffer) {

   /*  at the current file location, read a ses_word */

  ses_error_flag return_value = SES_NO_ERROR;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_read_word: Invalid file handle in ses_read_word\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_INVALID_FILE_HANDLE;
  }

  if (the_buffer == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_word: null word buffer\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (FILE_LIST[the_handle]->_the_setup->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_read_word:  setup not complete \n");
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_SETUP_ERROR;
  }

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_word: ses file handle null in ses_read_word\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  FILE* pFILE = _getPFILE(pSFH);
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_word: c file handle null in ses_read_word\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  /*  here, we're good to go */


  long nsig = FILE_LIST[the_handle]->_the_setup->_significant_digits;
  ses_boolean do_valid = FILE_LIST[the_handle]->_the_setup->_do_validation;

  double the_double_read = _read_double(pSFH, nsig, do_valid);
  if (the_buffer == (ses_word_reference)NULL) {
    the_buffer = malloc(sizeof(long)*1);
    if (the_buffer == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
      printf("ses_read_word:  memory allocation error \n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return SES_MEMORY_ALLOCATION_ERROR;
    }
  }
  *the_buffer = the_double_read;

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

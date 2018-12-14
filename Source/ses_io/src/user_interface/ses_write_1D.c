
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_error_flag ses_write_1D(ses_file_handle the_handle, ses_word_reference the_buffer, ses_number dim) {

 /*  at the current file location, write a 1D array */

  ses_error_flag return_value = SES_NO_ERROR;


  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_write_1D: invalid ses file handle in ses_write_1D\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_INVALID_FILE_HANDLE;
  }

  if (FILE_LIST[the_handle]->_the_setup->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_write_1D:  setup not complete \n");
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_SETUP_ERROR;
  }

  if (the_buffer == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_write_1D: null buffer in ses_write_1D\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  return_value = ses_write_next(the_handle, the_buffer, dim, "no_label");



  return return_value;
}

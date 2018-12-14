
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_error_flag ses_write_number(ses_file_handle the_handle, ses_number the_buffer) {

  /*  at the current file location, write a ses_number */

  ses_error_flag return_value = SES_NO_ERROR;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_write_number: File handle is not valid in ses_write_number\n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }

  struct _ses_setup* pSET = FILE_LIST[the_handle]->_the_setup;
  if (pSET->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_write_number:  setup incomplete in ses_write_number\n");
#endif
    return SES_SETUP_ERROR;
  }

  ses_word the_word = the_buffer * 1.0;
  ses_error_flag write_error = ses_write_next(the_handle, &the_word, 1, "no_label");

  if (write_error == SES_WRITE_ERROR) {
#ifdef DEBUG_PRINT
    printf("ses_write_number: return from ses_write_next is SES_FALSE in ses_write_number\n");
#endif
    return SES_WRITE_ERROR;
  }

  return return_value;
}


#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_error_flag ses_write_word(ses_file_handle the_handle, ses_word the_buffer) {

  /*  at the current file location, write a ses_word */

  ses_error_flag return_value = SES_NO_ERROR;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_write_word: File handle is not valid in ses_write_word\n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }

  struct _ses_setup* pSET = FILE_LIST[the_handle]->_the_setup;
  if (pSET->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_write_word: setup not complete\n");
#endif
    return SES_SETUP_ERROR;
  }

  ses_word the_word = the_buffer;
  ses_error_flag write_error = ses_write_next(the_handle, &the_word, 1, "no_label");

  if (write_error == SES_WRITE_ERROR) {
#ifdef DEBUG_PRINT
    printf("ses_write_word: return from ses_write_next is SES_FALSE \n");
#endif
    return SES_WRITE_ERROR;
  }


  return return_value;
}

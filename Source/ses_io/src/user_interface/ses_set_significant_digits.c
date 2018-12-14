
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

ses_error_flag ses_set_significant_digits(ses_file_handle the_handle, ses_number number_digits) {
  
  ses_error_flag return_value = SES_NO_ERROR;
  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_set_significant digits:invalid file handle \n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }

  struct _ses_setup* the_setup = FILE_LIST[the_handle]->_the_setup;
  if (the_setup == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_set_significant digits: setup null \n");
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_SETUP_ERROR;
  }

  if (the_setup->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_set_significant digits: setup not complete \n");
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_SETUP_ERROR;
  }

  if (number_digits < 0) {
#ifdef DEBUG_PRINT
    printf("ses_set_significant digits: invalid nsig \n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }


  ses_boolean didit_setup = _set_significant_digits(the_setup, number_digits);
  if (didit_setup == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_set_significant digits: set sig digits return false\n");
#endif
    return SES_SETUP_ERROR;
  }
  return return_value;
}

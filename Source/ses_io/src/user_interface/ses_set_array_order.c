
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_error_flag ses_set_array_order(ses_file_handle the_handle, ses_array_order the_order) {

  ses_error_flag return_value = SES_NO_ERROR;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_set_array_order: file handle invalid in ses_set_array_order \n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }

  struct _ses_setup* the_setup = FILE_LIST[the_handle]->_the_setup;
  if (the_setup == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_set_array_order: setup null in ses_set_array_order\n");
#endif
    return SES_SETUP_ERROR;
  }

  if (the_setup->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_set_array_order: setup not complete in ses_set_array_order\n");
#endif
    return SES_SETUP_ERROR;
  }

  ses_boolean didit_setup = _set_array_order(the_setup, the_order);
  if (didit_setup == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_set_array_order: set array order return false in ses_set_array_order\n");
#endif
    return SES_SETUP_ERROR;
  }

  return return_value;

}


#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_error_flag ses_set_label(ses_file_handle the_handle, ses_label the_label) {

  /*  set the label in the current data record */

  ses_error_flag return_value = SES_NO_ERROR;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_set_label: file handle invalid in ses_set_label \n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }

  struct _ses_data_record* pDR = FILE_LIST[the_handle]->_current_data_record;
  if (pDR == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_set_label: data record null in ses_set_label\n");
#endif
    return SES_WRITE_ERROR;
  }

  struct _ses_iterator* pIT = pDR->_the_iterator;
  if (pIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_set_label: iterator null in ses_set_label\n");
#endif
    return SES_WRITE_ERROR;
  }

  ses_boolean didit_return = _set_current_label(pIT, the_label);
  if (didit_return == SES_FALSE) {
    return_value = SES_FUNCTION_FAIL_ERROR;
  }
  return return_value;

}

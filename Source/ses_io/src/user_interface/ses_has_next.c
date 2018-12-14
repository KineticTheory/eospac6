
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_boolean ses_has_next(ses_file_handle the_handle) {

  /*  return whether the iterator has another array */
  /*  on error, this routine returns SES_FALSE */

  ses_boolean return_value = SES_FALSE;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_has_next: invalid ses file handle in ses_has_next\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_FALSE;
  }

  struct _ses_setup* pSET = FILE_LIST[the_handle]->_the_setup;
  if (pSET->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_has_next: setup not complete \n");
#endif
    return SES_FALSE;
  }

  /*  get the current data record */

  struct _ses_data_record* pDR = FILE_LIST[the_handle]->_current_data_record;
  if (pDR == (struct _ses_data_record*)NULL) {
    _set_latest_error(SES_READ_ERROR);
    return SES_FALSE;
  }

  /*  get the iterator */


  struct _ses_iterator* pIT = pDR->_the_iterator;
  if (pIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_has_next: iterator null in ses_has_next\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_FALSE;
  }

  /*  determine if there are more arrays  */

  if (pIT->_current_array < pIT->_number_arrays) {
    return_value = SES_TRUE;
  }

  return return_value;
}


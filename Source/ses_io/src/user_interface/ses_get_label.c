

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <string.h>

ses_label ses_get_label(ses_file_handle the_handle) {

  /*  return the label for the next array */
  /*  on error, this routine returns the SES_NULL_LABEL  */

  ses_label return_value = SES_NULL_LABEL;

  /*************************************************************************/

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_get_label: file handle not valid in ses_get_label\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_LABEL;
  }

  if (FILE_LIST[the_handle]->_the_setup->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_get_label:  setup not complete \n");
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_NULL_LABEL;
  }

  /*  get the current data record */
  struct _ses_data_record* pDR = FILE_LIST[the_handle]->_current_data_record;

  if (pDR == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_get_label: current data record null in ses_get_label\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_LABEL;
  }

  /*  get the iterator */
  struct _ses_iterator* the_iterator = pDR->_the_iterator;

  if (the_iterator == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_get_label: iterator is null in _get_current_label \n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_LABEL;
  }

  /***************************************************************/

  /*  everythings valid here, get the label */

  return_value = malloc(sizeof(char)*SES_MAX_LABEL_SIZE);
  strcpy(return_value, _get_current_label(the_iterator));
  if (return_value == (ses_label)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_get_label: Passing back null ses_label \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_LABEL;
  }
  return return_value;
}



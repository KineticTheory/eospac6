
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_error_flag ses_delete_next(ses_file_handle the_handle) {

#ifdef DEBUG_PRINT
  printf("ses_delete_next:  function has been removed from the user interface\n");
#endif
  _set_latest_error(SES_DELETE_ERROR);

  if (ses_is_valid(the_handle) == SES_FALSE) {
    return SES_DELETE_ERROR;
  }

  struct _ses_iterator* pIT = FILE_LIST[the_handle]->_current_data_record->_the_iterator;
  pIT->_current_array++;

  return SES_DELETE_ERROR;
}

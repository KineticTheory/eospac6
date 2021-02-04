
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#undef DEBUG_PRINT

ses_error_flag ses_set_date(ses_file_handle the_handle, long the_date) {


  ses_error_flag return_value = SES_NO_ERROR;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_set_date: file handle invalid in ses_set_date \n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }

  struct _ses_setup* the_setup = FILE_LIST[the_handle]->_the_setup;
  if (the_setup == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_set_date: setup null in ses_set_date\n");
#endif
    return SES_SETUP_ERROR;
  }

#ifdef DEBUG_PRINT
  printf("ses_set_date: setting to: %ld\n", the_date);
#endif

  the_setup->_date_changed = SES_TRUE;
  the_setup->_date = the_date;


  FILE_LIST[the_handle]->_output_date = the_date;

  return return_value;

}


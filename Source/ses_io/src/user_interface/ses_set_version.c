
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_error_flag ses_set_version(ses_file_handle the_handle, long the_version) {


  ses_error_flag return_value = SES_NO_ERROR;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_set_version: file handle invalid in ses_set_version \n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }

/*

  struct _ses_setup* the_setup = FILE_LIST[the_handle]->_the_setup;
  if (the_setup == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_set_version: setup null in ses_set_version\n");
#endif
    return SES_SETUP_ERROR;
  }

  the_setup->_version_changed = SES_TRUE;
  the_setup->_version = the_version;

*/

  FILE_LIST[the_handle]->_output_version = the_version;

  return return_value;

}


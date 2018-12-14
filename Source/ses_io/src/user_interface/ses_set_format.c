
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_error_flag ses_set_format(ses_file_handle the_handle, ses_file_type the_file_type) {


  ses_error_flag return_value = SES_NO_ERROR;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_set_format: file handle invalid in ses_set_format \n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }

 
  struct _ses_file_handle* ptHandle = FILE_LIST[the_handle]->_the_handle;
  if (ptHandle == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_set_format: _ses_file_handle* NULL\n");
#endif
    return SES_NULL_OBJECT_ERROR;
  }

  /*  put the new file type to write on the file handle */

  ptHandle->_filetype = the_file_type; 

  /*  change the function pointers for the file handle */

  _set_format_type(ptHandle, the_file_type);

  if (ptHandle->_filetype == 'Q') {
	return_value = SES_INVALID_OPEN_TYPE;
  }


  return return_value;

}



#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_file_handle ses_combine(ses_file_handle  file1, ses_file_handle file2, ses_string new_filename) {

  ses_file_handle return_value = 0;

  if (ses_is_valid(file1) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_combine:  file handle not valid, argument 1\n");
#endif
    return 0;
  }

  if (ses_is_valid(file2) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_combine:  file handle not valid, argument 2\n");
#endif
    return 0;
  }

  if (new_filename == (ses_string)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_combine:  invalid return filename \n");
#endif
    return 0;
  }

  /*  open and create the new filename */

  ses_file_handle the_handle = ses_open(new_filename, 'W');
  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;

  FILE_LIST[the_handle]->_the_setup->_setup_complete = SES_TRUE;
  FILE_LIST[the_handle]->_the_setup->_mid = FILE_LIST[file1]->_the_setup->_mid;
  FILE_LIST[the_handle]->_current_data_record = (struct _ses_data_record*)NULL;

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_combine:  file open error \n");
#endif
    _set_latest_error(SES_OPEN_ERROR);
    return 0;
  }

  if (FILE_LIST[the_handle]->FILE_TO_WRITE != (struct _ses_output_file*)NULL) {
	_destruct_output_file(FILE_LIST[the_handle]->FILE_TO_WRITE);
	free(FILE_LIST[the_handle]->FILE_TO_WRITE);
	FILE_LIST[the_handle]->FILE_TO_WRITE = (struct _ses_output_file*)NULL;
  }

  FILE_LIST[the_handle]->FILE_TO_WRITE = _combine_into_one(file1, file2);
  FILE_LIST[the_handle]->FILE_TO_WRITE->_ready_to_write = SES_TRUE;
  if (FILE_LIST[the_handle]->FILE_TO_WRITE == (struct _ses_output_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_combine:  NULL output file after combine\n");
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return 0;
  }
 
  return_value = the_handle;

  return return_value;
}

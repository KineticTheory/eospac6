
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_error_flag _read_array_binary(struct _ses_file_handle* pSFH, ses_word_reference the_buffer, long size, unsigned int nsig, ses_boolean do_valid) {

  ses_error_flag return_value = SES_NO_ERROR;


  /*  get the C file handle */

  /*****************************************************************************/
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_array_binary: ses file handle null\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  

  if (nsig < 0) {
#ifdef DEBUG_PRINT
    printf("_read_array_binary: nsig < 0\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }

  if (size < 0) {
#ifdef DEBUG_PRINT
    printf("_read_array_binary: size < 0\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }
  /*****************************************************************************/

  ses_word_reference the_read_buffer;
  the_read_buffer = _read_ses_word_array(pSFH, size, nsig, do_valid);



  if (the_read_buffer == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_array_binary:  _read_ses_word_array failed\n");
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return SES_FUNCTION_FAIL_ERROR;
  }

  int i=0;
  for (i=0; i<size; i++) {
    the_buffer[i] = the_read_buffer[i];
  }

  free(the_read_buffer);
  the_read_buffer = NULL;
 
  return return_value;
}


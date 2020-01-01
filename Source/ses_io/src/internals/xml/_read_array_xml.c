
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "tags.h"
#include "../xml_utilities.h"
#include <string.h>


ses_error_flag _read_array_xml(struct _ses_file_handle* pSFH, ses_word_reference the_buffer, long size ,unsigned int nsig, ses_boolean do_valid) {
 
  ses_error_flag return_value = SES_NO_ERROR;

  /*  this routine reads an array from the read pointer into the_buffer */


  /*  get the C file handle */

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_array_xml: ses file handle null\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

    
    /****************************
     * The following is always false. Unsigned int's can never be less than 0.
     *  Commenting out code: April 20, 2016

  if (nsig < 0) {
#ifdef DEBUG_PRINT
    printf("_read_array_xml: nsig < 0\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }
     *****************************/

  if (size < 0) {
#ifdef DEBUG_PRINT
    printf("_read_array_xml: size < 0\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }

  FILE* pFILE = pSFH->_c_file_handle;


  ses_word_reference the_read_buffer = _read_word_list_pFILE_xml(pFILE, size);
  if (the_read_buffer == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_array_ascii:  _read_word_list_xml failed\n");
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return SES_FUNCTION_FAIL_ERROR;
  }

  int i=0;
  for (i=0; i<size; i++) {
    the_buffer[i] = the_read_buffer[i];
  }

  free(the_read_buffer);
  the_read_buffer = (ses_word_reference)NULL;

 
  return return_value;
}

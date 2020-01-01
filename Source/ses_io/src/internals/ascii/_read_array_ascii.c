
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "_file_list_ascii.h"

#define check_errors_READ_ARRAY_ASCII HEADER(check_arrays_READ_ARRAY_ASCII)


ses_error_flag _read_array_ascii(struct _ses_file_handle* pSFH, ses_word_reference the_buffer, long size ,unsigned int nsig, ses_boolean do_valid) {

    /*  function prototypes  */

    ses_error_flag check_errors_READ_ARRAY_ASCII(struct _ses_file_handle* pSFH, long size ,unsigned int nsig);

    /*  end function prototypes */
 
    ses_error_flag return_value = SES_NO_ERROR;

    return_value = check_errors_READ_ARRAY_ASCII(pSFH, size, nsig);
    if (return_value != SES_NO_ERROR) {
	return return_value;
    }

    /*  read the ses word array */

    ses_word_reference the_read_buffer = _read_ses_word_array_ascii(pSFH, size, nsig, do_valid);
    if (the_read_buffer == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
      printf("_read_array_ascii:  _read_ses_word_array_ascii failed\n");
#endif
      _set_latest_error(SES_FUNCTION_FAIL_ERROR);
      return SES_FUNCTION_FAIL_ERROR;
    }


    //  copy the read buffer into the output buffer

    int i=0;
    for (i=0; i<size; i++) {
      the_buffer[i] = the_read_buffer[i];
    }

    //  free up memory and return

    free(the_read_buffer);
    the_read_buffer = (ses_word_reference)NULL;
 
    return return_value;
}

ses_error_flag check_errors_READ_ARRAY_ASCII(struct _ses_file_handle* pSFH, long size ,unsigned int nsig) {

  /*  error checks upon entrance to _read_array_ascii */

  /*****************************************************************************/
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_array_ascii: ses file handle null\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

    
    /****************************
     * The following is always false. Unsigned int's can never be less than 0.
     *  Commenting out code: April 20, 2016

  if (nsig < 0) {
#ifdef DEBUG_PRINT
    printf("_read_array_ascii: nsig < 0\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }
     ******************************/

  if (size < 0) {
#ifdef DEBUG_PRINT
    printf("_read_array_ascii: size < 0\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }
  /*****************************************************************************/

  return SES_NO_ERROR;

}


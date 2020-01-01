
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <stdio.h>


ses_error_flag ses_read_pairs(ses_file_handle the_handle, ses_word_reference buf1, ses_word_reference buf2, ses_number dim) {

  ses_error_flag return_value = SES_NO_ERROR;


  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_read_pairs: invalid file handle in ses_read_pairs\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_INVALID_FILE_HANDLE;
  }

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_pairs: null ses file handle in ses_read_pairs\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  /* FILE* pFILE = 0; */
  /* pFILE = */ _getPFILE(pSFH);

  if (FILE_LIST[the_handle]->_the_setup->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_read_pairs:  setup not complete \n");
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_SETUP_ERROR;
  }


  if (dim <= 0) {
#ifdef DEBUG_PRINT
    printf("ses_read_pairs: dim <= 0 in ses_read_pairs\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }

  if (buf1 == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_pairs: buf1 null in ses_read_pairs\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (buf2 == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_pairs: buf2 null in ses_read_pairs\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  /*  here, wer'e good to go */
  

  long nsig = FILE_LIST[the_handle]->_the_setup->_significant_digits;
  ses_boolean do_valid = FILE_LIST[the_handle]->_the_setup->_do_validation;

  int i;
  for (i=0; i < dim ; i++) {
    buf1[i]= _read_double(pSFH, nsig, do_valid);
    buf2[i]= _read_double(pSFH, nsig, do_valid);
  }

  _releasePFILE(pSFH);

  return return_value;

}

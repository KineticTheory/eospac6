

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_number ses_array_size_next(ses_file_handle the_handle) {

  /*  return the array size of the next array */

  ses_number return_value = SES_NUMBER_ARRAY_SIZE_ERROR;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_array_size_next: ses file handle invalid in ses_array_size_next\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_NUMBER_ARRAY_SIZE_ERROR;
  }

  /*  get the current data record */

  struct _ses_data_record* pDR = FILE_LIST[the_handle]->_current_data_record;
  if (pDR == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_array_size_next: current data record null in ses_array_size_next\n");
#endif
    _set_latest_error(SES_ARRAY_SIZE_ERROR);
    return SES_NUMBER_ARRAY_SIZE_ERROR;
  }

  /*  get the iterator*/

  struct _ses_iterator* pIT = pDR->_the_iterator;
  if (pIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_array_size_next: iterator null in ses_array_size_next\n");
#endif
    _set_latest_error(SES_ARRAY_SIZE_ERROR);
    return SES_NUMBER_ARRAY_SIZE_ERROR;
  }


  if (_is_valid_iterator(pIT)) {

    /*  return the size of the current array */

    return_value = pIT->_size_arrays[pIT->_current_array];
  }
  else {

#ifdef DEBUG_PRINT
    printf("ses_array_size_next: iterator not valid in ses_array_size_next \n");
#endif
    _set_latest_error(SES_ARRAY_SIZE_ERROR);
    return SES_NUMBER_ARRAY_SIZE_ERROR;
  }



  return return_value;

}



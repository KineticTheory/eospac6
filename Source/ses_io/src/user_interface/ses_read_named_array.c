
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


#include <string.h>

ses_error_flag ses_read_named_array(ses_file_handle the_handle, ses_label the_label, ses_word_reference buf1) {

  /*  function protoypes */

  ses_label ses_get_label(ses_file_handle the_handle);

  /*  end function prorotypes */
  
  /*  read a named array from the current table */

  ses_error_flag return_value = SES_NULL_OBJECT_ERROR;


  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_read_named_array: Invalid file handle in ses_read_named_array\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_INVALID_FILE_HANDLE;
  }

  if (FILE_LIST[the_handle]->_the_setup->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_read_named_array:  setup not complete \n");
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_SETUP_ERROR;
  }


  struct _ses_data_record* pDR = FILE_LIST[the_handle]->_current_data_record;
  if (pDR == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_named_array: Null current data record in ses_read_named array \n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }
  
  struct _ses_iterator* pIT = pDR->_the_iterator;
  if (pIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_named_array: Null iterator in ses_read_named array \n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  long dim = _get_array_size_with_label(pIT, the_label);
  if ((dim <= 0) && (the_label != (ses_label)NULL)) {
#ifdef DEBUG_PRINT
    printf("ses_read_named_array: dim is %ld <= 0 in ses_read_named_array with label %s\n", (long)dim, the_label);
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  if (buf1 == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_named_array: buf1 is null is ses_read_named_array \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  /*  here, wer're good to go */

  ses_label data_label = (ses_label)NULL;
  ses_error_flag read_error;
  long i;
  ses_word_reference buf1_tmp = (ses_word_reference)NULL;
  while (ses_has_next(the_handle) == SES_TRUE) {
    data_label = ses_get_label(the_handle);
    if ((the_label != (ses_label)NULL) && (strcmp(data_label, the_label) == 0)) {
      /*  found the array that corresponds to the_label */
      if (dim > 1) {
	return_value = SES_NO_ERROR;
        buf1_tmp = ses_read_next(the_handle);
        if (buf1_tmp == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
          printf("ses_read_named_array: ses_read_next error in ses_read_named_array\n");
#endif
          free(data_label);
          data_label = (ses_label)NULL;
          _set_latest_error(SES_READ_ERROR);
          return SES_READ_ERROR;
	}
        for (i=0; i < dim; i++) {
          buf1[i] = buf1_tmp[i];
	}
        
        free(buf1_tmp);
        buf1_tmp = (ses_word_reference)NULL;
      }
      else {
        ses_number buf2 = 0;
        ses_error_flag read_error = ses_read_number(the_handle, &buf2);
        if (read_error != SES_NO_ERROR) {
#ifdef DEBUG_PRINT
          printf("ses_read_named_array: read error (number) in ses_read_named_array\n");
#endif
          _set_latest_error(SES_READ_ERROR);
          free(data_label);
          data_label = (ses_label)NULL;
          return SES_READ_ERROR;
	}
        buf1[0] = (ses_word)(buf2*1.0);
      }
      break;
    }
    else {
      read_error = ses_skip(the_handle);
      if (read_error != SES_NO_ERROR) {
#ifdef DEBUG_PRINT
          printf("ses_read_named_array: skip error in ses_read_named_array\n");
#endif
          free(data_label);
          data_label = (ses_label)NULL;

          _set_latest_error(SES_READ_ERROR);
          return SES_READ_ERROR;
      }
    } 
    if (data_label != (ses_label)NULL) {
      free(data_label);
      data_label = (ses_label)NULL;
    }
  }
  if (data_label != (ses_label)NULL) {
    free(data_label);
    data_label = (ses_label)NULL;
  }


  return return_value;

}

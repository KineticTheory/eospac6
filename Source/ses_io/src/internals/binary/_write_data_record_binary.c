
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"



ses_error_flag _write_data_record_binary(struct _ses_data_record* tmp, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid) {

    ses_error_flag return_value = SES_NO_ERROR;


    if (tmp == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
      printf("_write_data_record_binary:  input data record null\n");
#endif
      _set_latest_error(SES_NULL_OBJECT_ERROR);
      return SES_NULL_OBJECT_ERROR;
    }

    if (tmp->_the_iterator == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
      printf("_write_data_record_binary:  table iterator null\n");
#endif
      _set_latest_error(SES_NULL_OBJECT_ERROR);
      return SES_NULL_OBJECT_ERROR;
    }

    long size = _get_number_arrays(tmp->_the_iterator);
    if (size <= 0) {
#ifdef DEBUG_PRINT
      printf("_write_data_record_binary: size < 0\n");
#endif
      _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
      return SES_OBJECT_OUT_OF_RANGE;
    }

    long array_size;
    long index;
    ses_boolean didit_write = SES_FALSE;

    for (index=0;index < size; index++) {

	array_size = _get_array_size(tmp->_the_iterator, index);


	if (array_size <= 0) {
#ifdef DEBUG_PRINT
	  int mid = tmp->_mid;
	  int tid = tmp->_tid;
          printf("_write_data_record_binary: Invalid array size = %ld index = %ld mid = %d tid = %d\n", array_size, index, mid, tid);
#endif
          _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
	  return SES_OBJECT_OUT_OF_RANGE;
	}

        if (array_size > 1) {
	  ses_boolean original_needs_flip = pSFH->_needs_flip;
	  if ((tmp->_tid > 100) && (tmp->_tid <= 199)) {
		pSFH->_needs_flip = SES_FALSE;
	  }

          didit_write = _write_ses_word_array(pSFH, &tmp->_the_data[index][0], array_size, nsig, do_valid);
	  pSFH->_needs_flip = original_needs_flip;
          if (didit_write == SES_FALSE) {
#ifdef DEBUG_PRINT
            printf("_write_data_record_binary: write error -- returning SES_WRITE_ERROR\n");
#endif
            _set_latest_error(SES_WRITE_ERROR);
	    return SES_WRITE_ERROR;
	  }
	  else {
	    return_value = SES_NO_ERROR;
	  }

        }
        else {

          if (tmp->_the_data == (ses_word_reference*)NULL) {
#ifdef DEBUG_PRINT
            printf("_write_data_record_binary  data is NULL -- returning SES_NULL_OBJECT_ERROR\n");
#endif
            _set_latest_error(SES_NULL_OBJECT_ERROR);
            return SES_NULL_OBJECT_ERROR;
 
	  }

          didit_write = _write_double(pSFH, tmp->_the_data[index][0], 0, SES_FALSE);
          if (didit_write == SES_FALSE) {
#ifdef DEBUG_PRINT
            printf("_write_data_record_binary: write error on _write_double -- returning SES_WRITE_ERROR\n");
#endif
            _set_latest_error(SES_WRITE_ERROR);
	    return SES_WRITE_ERROR;
	  }
        }
    }


    return return_value;

}

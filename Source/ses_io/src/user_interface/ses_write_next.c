

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdlib.h"

#include <string.h>

#undef DEBUG_SES_WRITE_NEXT

ses_error_flag ses_write_next(ses_file_handle the_handle, ses_word_reference the_buffer, ses_number dim, ses_label the_label) {
  
  /*  add data to a table in the current_data_record */

  ses_error_flag return_value = SES_NO_ERROR;

#ifdef DEBUG_SES_WRITE_NEXT
  printf("ses_write_next:  dim is %d\n", dim);
#endif


  /*  error check the arguments */

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_write_next: invalid ses file handle in ses_write_next\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_INVALID_FILE_HANDLE;
  }

  struct _ses_setup* pSET = FILE_LIST[the_handle]->_the_setup;
  if (pSET->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_write_next: ses setup not complete in ses_write_next\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_SETUP_ERROR;
  }

  if (the_buffer == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_write_next: buffer null \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  struct _ses_data_record* pDR = FILE_LIST[the_handle]->_current_data_record;
  if (pDR == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_write_next: Current data record is null in ses_write_next\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (the_label == (ses_string)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_write_next:  Label null in ses_write_next\n");
#endif	
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
   

  }

  struct _ses_iterator* pIT = pDR->_the_iterator;
  if (pIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_write_next: iterator is null in ses_write_next\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (pDR->_the_data == (ses_word_reference*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_write_next: data is null in ses_write_next\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (pIT->_label_arrays == NULL) {
#ifdef DEBUG_PRINT
    printf("ses_write_next: label arrays null in ses_write_next\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (pIT->_size_arrays == NULL) {
#ifdef DEBUG_PRINT
    printf("ses_write_next: size arrays null in ses_write_next\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (pIT->_address_arrays == NULL) {
#ifdef DEBUG_PRINT
    printf("ses_write_next: address arrays null in ses_write_next\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  /*  Here, we're good to go */
  FILE* pFILE = 0;
  pFILE = _getPFILE(FILE_LIST[the_handle]->_the_handle);


  long current = pIT->_current_array;

  if (current >= pIT->_number_arrays) {

#ifdef DEBUG_PRINT
    printf("ses_write_next:  current %ld >= number_arrays %ld\n", current, pIT->_number_arrays);
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }

  if (dim <= 0) {
#ifdef DEBUG_PRINT
    printf("ses_write_next:  dim <= 0 in ses_write_next\n");
#endif
    pIT->_current_array = pIT->_number_arrays;
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }
  pIT->_size_arrays[current] = dim;
  if (current == 0) {
    pIT->_address_arrays[current] = 0;
  }
  else {
    pIT->_address_arrays[current] = pIT->_address_arrays[current - 1] + dim;
  }

#ifdef DEBUG_SES_WRITE_NEXT_HELLO
  printf("ses_write_next -- current is %d dim is %d\n", dim);
#endif
  pDR->_the_data[current] = malloc(8*dim);
#ifdef DEBUG_SES_WRITE_NEXT_HELLO
  printf("ses_write_next -- after malloc\n");
#endif


  if (pDR->_the_data[current] == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_write_next: memory allocation error in ses_write_next\n");
#endif
    pIT->_current_array = pIT->_number_arrays + 1;
    _set_latest_error(SES_WRITE_ERROR);
    return SES_WRITE_ERROR;
  }
  int i = 0;

  if (the_buffer == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_write_next: the_buffer is null in ses_write_next\n");
#endif
    pIT->_current_array = pIT->_number_arrays + 1;
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  for (i=0; i < dim; i++) {
     pDR->_the_data[current][i] = the_buffer[i];
  }
  

  long nsig = 0;
  nsig = FILE_LIST[the_handle]->_the_setup->_significant_digits;
  ses_boolean do_valid = SES_FALSE;
  do_valid = FILE_LIST[the_handle]->_the_setup->_do_validation;



  pDR->_has_data = SES_TRUE;

  
  strcpy(pIT->_label_arrays[current], the_label);
   
    
  pIT->_current_array++;
  _releasePFILE(FILE_LIST[the_handle]->_the_handle);

  return return_value;
}

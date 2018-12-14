
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

ses_error_flag ses_skip(ses_file_handle the_handle) {

  /*  skip the next array in the table */

  ses_error_flag return_value = SES_NO_ERROR;


  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_skip: invalid file handle in ses_skip\n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }

  if (FILE_LIST[the_handle]->_the_setup->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_skip:  setup not complete \n");
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_SETUP_ERROR;
  }
 
  /*  get the iterator */

  struct _ses_data_record* pDR = FILE_LIST[the_handle]->_current_data_record;
  if (pDR == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_skip: ses data record null in ses_skip\n");
#endif
    return SES_READ_ERROR;
  }

  struct _ses_iterator* pIT = _get_iterator(pDR);
  if (pIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_skip: ses iterator null in ses_skip\n");
#endif
     return SES_READ_ERROR;
  }

  /*  get the C file handle */

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_skip: ses file handle null in ses_read_next\n");
#endif
    pIT->_current_array = pIT->_number_arrays;
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  FILE* pFILE = _getPFILE(pSFH);
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_skip: c file handle null\n");
#endif
    pIT->_current_array = pIT->_number_arrays;
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  struct _ses_setup* pSET = FILE_LIST[the_handle]->_the_setup;
  if (pSET == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_skip: setup null\n");
#endif
    pIT->_current_array = pIT->_number_arrays;
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  /*  get the setup information */

  ses_material_id the_mid = pSET->_mid;
  ses_table_id the_tid = pSET->_tid;

  /*  get the directory information */

  struct _ses_directory* pDIR = FILE_LIST[the_handle]->_directory;
  if (pDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_skip: directory null\n");
#endif
    pIT->_current_array = pIT->_number_arrays;
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  struct _ses_index_record* pIR = FILE_LIST[the_handle]->_current_index_record;
  if (pIR == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_skip: current index record nul \n");
#endif
    pIT->_current_array = pIT->_number_arrays;
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }
 
  /*  here, we're good to go */

  /* increment the iterator */

  pIT->_current_array++;
  if (pIT->_current_array > pIT->_number_arrays) {
#ifdef DEBUG_PRINT
    printf("ses_skip: iterator current_array exceeds number arrays in ses_skip\n");
#endif
    return SES_READ_ERROR;
  }

  /*  get the current array index */

  long current = pIT->_current_array;

  /*  get the table location */

  long madr = _get_address_for_material(pDIR, the_mid, pSFH);
  long tadr = _get_address_for_table(pIR, the_tid, pSFH);
  long start = madr+tadr;

  /*  get the address to the next array */

  long iadr;
  if (current < pIT->_number_arrays) {
    iadr = pIT->_address_arrays[current];
  }
  else {
    iadr = pIT->_address_arrays[current-1] + pIT->_size_arrays[current-1]*8;
  }


  /*  go to the next array */
  int fseek_return = fseek(pFILE, start + iadr, SEEK_SET);
  if (fseek_return != 0) {
#ifdef DEBUG_PRINT
    printf("ses_skip: fseek return\n");
#endif
    return SES_FUNCTION_FAIL_ERROR;
  }

  _releasePFILE(pSFH);

  return return_value;
}

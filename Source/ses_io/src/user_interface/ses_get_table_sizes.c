
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <stdlib.h>


ses_table_sizes_reference ses_get_table_sizes(ses_file_handle the_handle, ses_material_id mid, long* size) {

  /*  return the table sizes's for the given material */
  
  ses_table_id_reference return_value = (ses_table_id_reference)NULL;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_get_table_sizes: file handle not valid\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_TABLES;
  }

  if ((FILE_LIST[the_handle]->_the_setup)->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_get_table_sizes: not setup\n");
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_NULL_TABLES;
  }

  struct _ses_directory* ptDIR = FILE_LIST[the_handle]->_directory;

  ses_boolean isit_there = _check_directory_for_material(ptDIR, mid);
  if (isit_there == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_get_table_sizes: material_id %ld not in file\n", mid);
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_TABLES;
  }

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_get_table_sizes: ses file handle null\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_TABLES;
  }

  FILE* pFILE = _getPFILE(pSFH);

  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_get_table_sizes: c file handle null\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_TABLES;
  }

  if (size == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_get_table_sizes: size null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_TABLES;
  }

  /*  here, everything is valid */
#ifdef DEBUG_SGTS
  printf("ses_get_table_sizes:  got past error checking\n");
#endif

  long current_position = ftell(pFILE);
  ses_boolean didit_go = pSFH->pt2_go_to_index_record(the_handle, mid, 0);
  if (didit_go == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_get_table_sizes: could not find index record\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_TABLES;
  }

  long offset = _get_address_for_material(ptDIR, mid, pSFH);

  struct _ses_index_record* pIR = _read_index_record(pSFH, offset);
  if (pIR == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_get_table_sizes: Could not read index record\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_TABLES;
  }

  return_value = _get_nwds_index_record(pIR);
  *size = pIR->_nrec;


  rewind(pFILE);
  int fseek_return = fseek(pFILE, current_position, SEEK_SET);
  if (fseek_return != 0) {
#ifdef DEBUG_PRINT
    printf("ses_get_table_sizes: fseek error\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_TABLES;
  }

  _releasePFILE(pSFH);

#ifdef DEBUG_SGTS
  printf("ses_get_table_sizes:  got to end return_value is %ld\n", return_value);
#endif
 
  return return_value;
}



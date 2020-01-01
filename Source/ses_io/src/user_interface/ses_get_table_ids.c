
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <stdlib.h>

#undef DEBUG_PRINT

ses_table_id_reference ses_get_table_ids(ses_file_handle the_handle, ses_material_id mid, long* size) {

  /*  return the table id's for the given material */
  
  ses_table_id_reference return_value = (ses_table_id_reference)NULL;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_get_table_ids: file handle not valid in ses_get_table_ids\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_TABLES;
  }

  /*   changed to setup to the 201 or the 101 table if false */

  //struct _ses_index_record* original = _copy_index_record(FILE_LIST[the_handle]->_current_index_record);
  if ((FILE_LIST[the_handle]->_the_setup)->_setup_complete == SES_FALSE) {
  /*   changed to setup to the 201 table if false */

	ses_error_flag didit_setup = ses_setup(the_handle, mid, 201);
	if (didit_setup != SES_NO_ERROR) {

	   didit_setup = ses_setup(the_handle, mid, 101);
	   if (didit_setup != SES_NO_ERROR) {
#ifdef DEBUG_PRINT
           	printf("ses_get_table_ids:  not setup and did not set up to 201 table for mid %ld\n", (long)mid);
#endif
	   	_set_latest_error(didit_setup);
	   	return SES_NULL_TABLES;

	   }   
	}

  }


  struct _ses_directory* ptDIR = FILE_LIST[the_handle]->_directory;

  ses_boolean isit_there = _check_directory_for_material(ptDIR, mid);
  if (isit_there == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_get_table_ids: material_id %ld not in file, from ses_get_table ids\n", mid);
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_TABLES;
  }

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_get_table_ids: ses file handle null in ses_get_table_ids\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_TABLES;
  }

  FILE* pFILE = _getPFILE(pSFH);
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_get_table_ids: c file handle null in ses_get_table_ids\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_TABLES;
  }

  if (size == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_get_table_ids: size null in ses_get_table_ids\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_TABLES;
  }

  /*  here, everything is valid */

  long current_position = ftell(pFILE);
  ses_boolean didit_go = pSFH->pt2_go_to_index_record(the_handle, mid, 0);
  if (didit_go == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_get_table_ids: could not find index record in ses_get_table_ids\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_TABLES;
  }

  long offset = _get_address_for_material(ptDIR, mid, pSFH);
  struct _ses_index_record* pIR = _read_index_record(pSFH, offset);
  if (pIR == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_get_table_ids: Could not read index record in ses_get_table_ids\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_TABLES;
  }

  ses_table_id_reference almost_return_value = _get_tblid(pIR);
  return_value = malloc(sizeof(ses_table_id)*pIR->_nrec);
  int i = 0;
  for (i = 0; i < pIR->_nrec; i++) {
     return_value[i] = almost_return_value[i];
  }
  
  *size = pIR->_nrec;

  rewind(pFILE);
  int fseek_return = fseek(pFILE, current_position, SEEK_SET);
  if (fseek_return != 0) {
#ifdef DEBUG_PRINT
    printf("ses_get_table_ids: fseek error in ses_get_table_ids\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_TABLES;
  }

  _destruct_ses_index_record(pIR);
  free(pIR);
  pIR = (struct _ses_index_record*)NULL;

  _releasePFILE(pSFH); 

  return return_value;
}



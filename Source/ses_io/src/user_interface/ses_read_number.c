
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_error_flag ses_read_number(ses_file_handle the_handle, ses_number_reference the_buffer) {

  /*  at the current file location, read a ses_number */

  ses_error_flag return_value = SES_NO_ERROR;

  /*  function prototypes */

  ses_error_flag check_errors_SES_READ_NUMBER(ses_file_handle the_handle, ses_number_reference the_buffer);

  /*  end function prototypes */

  return_value = check_errors_SES_READ_NUMBER(the_handle, the_buffer);
  if (return_value != SES_NO_ERROR) {
	return return_value;
  }

  /*  open the file handle */

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  FILE* pFILE = (FILE*)NULL;
  pFILE = _getPFILE(pSFH);

  /*  get the directory */

  struct _ses_directory* pDIR = FILE_LIST[the_handle]->_directory;

  /*  read the number */

  long the_long_read = 0;

  ses_table_id tid = FILE_LIST[the_handle]->_the_setup->_tid;
  int table_index = _get_table_index(FILE_LIST[the_handle]->_current_index_record, tid);
  if (pDIR->_has_multiple_files == SES_TRUE) {

    //  set the array address and filename

    pSFH->_array_address = FILE_LIST[the_handle]->_current_index_record->_array_iadr[table_index][pSFH->_iteration_index];
    pSFH->_array_filename = FILE_LIST[the_handle]->_current_index_record->_array_filename[table_index][pSFH->_iteration_index];

    //  go to the array address 

    long location = pSFH->_array_address;
    ses_boolean didit_go = pSFH->pt2_go_to_next_array_location(pSFH, location);  

    if (didit_go == SES_FALSE) {
	int index = _get_table_index(FILE_LIST[the_handle]->_current_index_record, tid);
	if (pSFH->_iteration_index - 1 <= 1) {
		if (pSFH->_iteration_index - 1 == 0) {
			the_long_read = FILE_LIST[the_handle]->_current_index_record->_nr[index];
		}
		if (pSFH->_iteration_index - 1 == 1) {
			the_long_read = FILE_LIST[the_handle]->_current_index_record->_nt[index];
		}
	}
	else {
		the_long_read = _read_long(pSFH);
	}
    }
  }
  else {
    /*  read from the current location */
    long location = ftell(pSFH->_c_file_handle);
    ses_boolean didit_go = SES_FALSE;
    didit_go = pSFH->pt2_go_to_next_array_location(pSFH, location); 
    the_long_read = _read_long(pSFH);
  }

  /*  copy the number into the output buffer */

  if (the_buffer == (ses_number_reference)NULL) {
    the_buffer = malloc(sizeof(long)*1);
    if (the_buffer == (ses_number_reference)NULL) {
#ifdef DEBUG_PRINT
      printf("ses_read_number:  memory allocation error \n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return SES_MEMORY_ALLOCATION_ERROR;
    }
  }
  *the_buffer = the_long_read;

  /*  code added to move the iterator along -- found while testing wrappers */
  
  struct _ses_iterator* pIT = FILE_LIST[the_handle]->_current_data_record->_the_iterator;

  if (pIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_number:  iterator null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }
  
  pIT->_current_array++;

  /*  release the FILE* object and return */

  _releasePFILE(pSFH);
  return return_value;

}


ses_error_flag check_errors_SES_READ_NUMBER(ses_file_handle the_handle, ses_number_reference the_buffer) {

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_read_number: invalid file handle in ses_read_number\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_INVALID_FILE_HANDLE;
  }


  if (the_buffer == (ses_number_reference)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_number: null number buffer\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (FILE_LIST[the_handle]->_the_setup->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_read_number:  setup not complete \n");
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_SETUP_ERROR;
  }
  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_number: ses file handle null in ses_read_number\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  struct _ses_directory* pDIR = FILE_LIST[the_handle]->_directory;
  if (pDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_number: ses directory null in ses_read_number\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }


  return SES_NO_ERROR;
}


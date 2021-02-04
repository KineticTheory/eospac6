

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#define check_errors_SES_READ_NEXT HEADER(check_errors_SES_READ_NEXT)

#undef DEBUG_PRINT

ses_word_reference ses_read_next(ses_file_handle the_handle) {

 //  function prototypes

  ses_error_flag check_errors_SES_READ_NEXT(ses_file_handle the_handle);

  //  end function prototypes

  ses_error_flag error_check = check_errors_SES_READ_NEXT(the_handle);
  if (error_check != SES_NO_ERROR) {
	return (ses_word_reference)NULL;
  }

  /*  read the next array in the table */

  ses_word_reference  the_buffer = (ses_word_reference)NULL;

  struct _ses_data_record* pDR = FILE_LIST[the_handle]->_current_data_record;
  struct _ses_iterator* pIT = _get_iterator(pDR);
  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  FILE* pFILE = _getPFILE(pSFH);
  struct _ses_setup* pSET = FILE_LIST[the_handle]->_the_setup;
  struct _ses_directory* pDIR = FILE_LIST[the_handle]->_directory;
  struct _ses_index_record* pIR = FILE_LIST[the_handle]->_current_index_record;

  ses_material_id the_mid = pSET->_mid;
  ses_table_id the_tid = pSET->_tid;

  /*  here, we're good to go */

  /*  get the current array index */

  long current = pIT->_current_array;

  /*  get the material, table location */

  long madr = _get_address_for_material(pDIR, the_mid, pSFH);
  long tadr = _get_address_for_table(pIR, the_tid, pSFH);
  long start = 0;
  start = madr+tadr;

  /*  get the address to the next array */

  long iadr = 0;
  iadr = pIT->_address_arrays[current];

  /* ses_boolean didit_go = SES_FALSE; */

  /*  go to the next array location */

  ses_table_id tid = FILE_LIST[the_handle]->_the_setup->_tid;
  int table_index = _get_table_index(FILE_LIST[the_handle]->_current_index_record, tid);
  if (pDIR->_has_multiple_files == SES_TRUE) {

       //  set the file and address information for multiple files onto the current index record

       pSFH->_array_address = FILE_LIST[the_handle]->_current_index_record->_array_iadr[table_index][pSFH->_iteration_index];
       pSFH->_array_filename = FILE_LIST[the_handle]->_current_index_record->_array_filename[table_index][pSFH->_iteration_index];

       /* didit_go = */ _go_to_next_array_location(pSFH, pSFH->_array_address);

  }
  else {

       /* didit_go = */ _go_to_next_array_location(pSFH, start + iadr);

  }

#ifdef DEBUG_PRINT
    printf("ses_read_next: tid: %d\n", the_tid);
#endif
  /*  create the output buffer */

  long the_size = pIT->_size_arrays[current];
  if (the_size <= 0) {
#ifdef DEBUG_PRINT
    printf("ses_read_next: array size error = %ld in ses_read_next current is %ld\n", the_size, current);
#endif
    pIT->_current_array = pIT->_number_arrays;
    _set_latest_error(SES_READ_ERROR);
    return (ses_word_reference)NULL;
  }
    
#ifdef DEBUG_PRINT
    printf("ses_read_next: the size to read: %ld\n", the_size);
#endif

  the_buffer = malloc(sizeof(ses_word)*the_size);
  int i = 0;
  for (i= 0; i < the_size; i++) {
	the_buffer[i] = 0.0;
  }
  if (the_buffer == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_next: memory allocation error in ses_read_next\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return (ses_word_reference)NULL;
  }

  
  /*  fill the output buffer */

  long nsig = FILE_LIST[the_handle]->_the_setup->_significant_digits;
  ses_boolean do_valid = FILE_LIST[the_handle]->_the_setup->_do_validation;

  //  pad comments tables if necessary

  if ((the_tid >= 100) && (the_tid < 200) && (pSFH->_filetype != BINARY_TYPE) && (the_size > 1)) {

     //  comments tables -- need to be padded to mod 8

     int i = 0;
     char tmp;
     
     int dim = the_size;
     int pad = (dim%8 > 0) ? (8-dim%8) : 0;
     dim = dim + pad;

     ses_string the_string = malloc(sizeof(char)*dim);
     int i3 = 0;
     for (i3= 0; i3 < sizeof(char)*dim; i3++) {
	the_string[i3] = ' ';
     }
     for (i=0; i < the_size ; i++) {

        tmp = pSFH->pt2_read_char(pFILE);
        the_string[i] = tmp;
    
      }
      for (i = the_size; i < dim; i++) {
	the_string[i] = ' ';
      }
      the_string[dim-1] = '\0';
      free(the_buffer);
      the_buffer = (ses_word_reference)&the_string[0];



  }
  else {


     ses_boolean original_needs_flip = pSFH->_needs_flip;
     if ((the_tid > 100) && (the_tid < 200)) {
       pSFH->_needs_flip = SES_FALSE;  
     }
     ses_error_flag didit_read = pSFH->pt2_read_array(pSFH, the_buffer, the_size, nsig, do_valid);
     if (didit_read != SES_NO_ERROR) {
#ifdef DEBUG_PRINT
        printf("ses_read_next:  _read_array error\n");
#endif
        pSFH->_needs_flip = original_needs_flip;
        _set_latest_error(SES_READ_ERROR);
        return (ses_word_reference)NULL;
     }
     pSFH->_needs_flip = original_needs_flip;

  }
  if (pDIR->_has_multiple_files == SES_TRUE) {

  	if (pIT->_current_array == 0) {
 		the_buffer[0] = pIR->_nr[table_index];
 	 }
 	 if (pIT->_current_array == 1) {
 		the_buffer[0] = pIR->_nt[table_index];
 	 } 
  }

  /* increment the iterator */
  pIT->_current_array++;

  _releasePFILE(pSFH);
 
  return the_buffer;
}

ses_error_flag check_errors_SES_READ_NEXT(ses_file_handle the_handle) {

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_read_next: invalid handle in ses_read_next\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }


  if (FILE_LIST[the_handle]->_the_setup->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_read_next:  setup not complete \n");
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_SETUP_ERROR;
  }

  struct _ses_data_record* pDR = FILE_LIST[the_handle]->_current_data_record;
  if (pDR == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_next: current data record null in ses_read_next\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  /*  get the iterator */

  struct _ses_iterator* pIT = _get_iterator(pDR);
  if (pIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_next: iterator null in ses_read_next\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  /*  get the C file handle */

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_next: ses file handle null in ses_read_next\n");
#endif
    pIT->_current_array = pIT->_number_arrays;
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  FILE* pFILE = _getPFILE(pSFH);
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_next: c file handle null in ses_read_next\n");
#endif
    pIT->_current_array = pIT->_number_arrays;
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  struct _ses_setup* pSET = FILE_LIST[the_handle]->_the_setup;
  if (pSET == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_next: ses setup null in ses_read_next\n");
#endif
    pIT->_current_array = pIT->_number_arrays;
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }
  struct _ses_directory* pDIR = FILE_LIST[the_handle]->_directory;
  if (pDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_next: directory null in ses_read_next\n");
#endif
    pIT->_current_array = pIT->_number_arrays;
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }  

  struct _ses_index_record* pIR = FILE_LIST[the_handle]->_current_index_record;
  if (pIR == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_next: current index record null in ses_read_next \n");
#endif
    pIT->_current_array = pIT->_number_arrays;
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  if (pSFH->pt2_read_array == NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_next:  function pointer null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }
  

  return SES_NO_ERROR;
}


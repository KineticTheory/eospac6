
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <string.h>

#include <stdio.h>
#include <stdlib.h>

#define _is_valid_tid_for_comments HEADER(_is_valid_tid_for_comments)


ses_error_flag ses_comments(ses_file_handle the_handle, ses_string* the_string) {

  /*  at the current file location, read a 1D array of char's and 
      return it in the string */

  /*  function prototypes */

  ses_boolean _is_valid_tid_for_comments(ses_table_id the_tid);

  /*  end funciton prototypes */

  ses_error_flag return_value = SES_NO_ERROR;
  ses_string the_new_string = (ses_string)NULL;


  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_comments: ses file handle not valid in ses_comments\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_INVALID_FILE_HANDLE;
  }

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_comments: _ses_file_handle NULL in ses_comments\n"); 
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  struct _ses_setup* pSET = FILE_LIST[the_handle]->_the_setup;
  if (pSET->_setup_complete ==  SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_comments: setup not complete in ses_comments\n"); 
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_SETUP_ERROR;
  }

  FILE* pFILE = _getPFILE(pSFH);
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_comments: null FILE* in ses_comments\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }



  ses_material_id the_mid = pSET->_mid;
  if (_is_valid_mid(the_mid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_comments: invalid material = %ld id in setup block, in ses_comments\n",pSET->_mid);
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }
  

  /*  go to the 101 or 102  or 103 table for the current material */

  ses_table_id the_tid = pSET->_tid;

  if (_is_valid_tid_for_comments(the_tid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_comments: invalid tid in setup block, in ses_comments, tid is %d\n", (int)the_tid);
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  ses_boolean didit_go = pSFH->pt2_go_to_data_record(the_handle, the_mid, the_tid);
  if (didit_go == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_comments: didit_go failed in ses_comments\n");
#endif
    _set_latest_error(SES_COMMENTS_ERROR);
    return SES_COMMENTS_ERROR;
  }

  struct _ses_index_record* pIR = FILE_LIST[the_handle]->_current_index_record;
  if (pIR == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_comments: current index record null in ses_comments\n");
#endif
    _set_latest_error(SES_COMMENTS_ERROR);
    return SES_COMMENTS_ERROR;
  }

  long dim = 0;
  if (pSFH->_filetype != ASCII_TYPE) {
  	dim = _get_table_size(pIR, the_tid)*8;
  }
  else {
  	dim = _get_table_size(pIR, the_tid);	
  }


  if (dim <= 0) {
#ifdef DEBUG_PRINT
    printf("ses_comments: table dimension <= 0 in ses_comments \n");
#endif
    _set_latest_error(SES_COMMENTS_ERROR);
    return SES_COMMENTS_ERROR;
  }

  if (the_string == (ses_string*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_comments:  the_string is NULL\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);   
    return SES_NULL_OBJECT_ERROR;
  }

  if (*the_string != (ses_string)NULL) {


#ifdef DEBUG_PRINT
      printf("ses_comments: *the_string is NOT NULL \n");
#endif
      _set_latest_error(SES_COMMENTS_ERROR);
      return SES_COMMENTS_ERROR;


  }

  /*  allocate the string */

  the_new_string = malloc(sizeof(char)*(dim +1 ));

  /*  everything is valid, ready to get the comments */

  int i;


  if (pSFH->_filetype == ASCII_TYPE) {
  	char tmp;
  	int index = 0;
  	int number_lines = (dim/80) + 1;
        int odim = dim;
	dim = dim + number_lines;
  	for (i=0; i < dim ; i++) {

  	  tmp = pSFH->pt2_read_char(pFILE);
	  if (tmp != '\n') {
  	  	the_new_string[index] = tmp;
		index++;
	  }
    
  	}
  	the_new_string[odim] = '\0';
  }
  else {
  	char tmp;
  	for (i=0; i < dim ; i++) {

  	  tmp = pSFH->pt2_read_char(pFILE);
  	  the_new_string[i] = tmp;
    
  	}
  	the_new_string[dim] = '\0';
  }
	


  *the_string = the_new_string;

  _releasePFILE(pSFH);

 
  return return_value;  
}

ses_boolean _is_valid_tid_for_comments(ses_table_id the_tid) {

  ses_boolean return_value = SES_FALSE;
  if (the_tid > 100 && the_tid < 200) {
    return_value = SES_TRUE;
  }
  return return_value;
}



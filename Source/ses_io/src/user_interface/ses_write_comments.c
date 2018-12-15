
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <string.h>

//#define DEBUG_SES_WRITE_COMMENTS
#define FIX_SES_WRITE_ASCII


ses_error_flag ses_write_comments(ses_file_handle the_handle, ses_string the_comments, ses_number dim) {

 /*  at the current file location, write a 1D array that holds the string 
     with the comments */
#ifdef DEBUG_SES_WRITE_COMMENTS
  printf("ses_write_comments:  the_comments are %s dim is %d\n", the_comments, dim);
#endif


  ses_error_flag return_value = SES_NO_ERROR;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_write_comments: invalid ses file handle in ses_write_comments\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_INVALID_FILE_HANDLE;
  }

  if (FILE_LIST[the_handle]->_the_setup->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_write_comments:  setup not complete \n");
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_SETUP_ERROR;
  }

  if (the_comments == (ses_string)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_write_comments: null buffer in ses_write_comments\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (dim <= 0) {
#ifdef DEBUG_PRINT
    printf("ses_write_comments: number of chars to write <= 0\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }


  int div = 8;


  /*  if you have a comment table, the comments must be padded to a factor of 8 */

  union char_union {
    ses_word the_word;
    char the_char[8];
  } myUnion;

  int odim = dim;

  //  if not ascii, pad up
  char my_filetype = FILE_LIST[the_handle]->_the_handle->_filetype;

  if (my_filetype == 'A') {
	div = 8;
  }

#ifdef FIX_SES_WRITE_ASCII
  	dim = dim + (8 - dim%8);
#else
  if (my_filetype != 'A') {

  	dim = dim + (8 - dim%8);
  }
#endif

  /* added by KT (malloc and free) */
  ses_word *the_buffer;
  the_buffer = (ses_word *)malloc(sizeof(ses_word) * dim / div);

  /* ses_word the_buffer[dim/div]; */
  int j2 = 0;
  for (j2=0; j2 < dim/div; j2++) {
	the_buffer[j2] = 0.0;
  }

  int index = 0;
  int jindex = 0;
  int i = 0;
  for (i=0; i < dim; i++) {
    if (index == 8) {
      index = 0;
      the_buffer[jindex] = myUnion.the_word;
      jindex++;
      if (i >= odim) {
         myUnion.the_char[index] = ' ';
      }
      else {
         myUnion.the_char[index] = the_comments[i];
      }
      index++;
    }
    else {
      if (i >= odim) {
         myUnion.the_char[index] = ' ';
      }
      else {
        myUnion.the_char[index] = the_comments[i];
      }
      index++;
    }
  }

#ifdef FIX_SES_WRITE_ASCII
  if (my_filetype == 'A') {
  	myUnion.the_char[7] = ' ';
  }
  else {
	myUnion.the_char[7] = '\0';
  }
#else
  myUnion.the_char[7] = '\0';
#endif
  the_buffer[jindex] = myUnion.the_word;
  ses_word_reference the_wbuffer = &the_buffer[0];



  ses_error_flag didit_write = SES_FALSE;


#ifdef DEBUG_SES_WRITE_COMMENTS
  printf("ses_write_comment:  passing |%s| to ses_write_next, strlen is %d (dim/div) is %d\n", the_wbuffer, strlen(the_wbuffer), dim/div);
  
#endif

  didit_write = ses_write_next(the_handle, the_wbuffer, (dim)/div, "no_label");

  free(the_buffer);

  return return_value;
}


#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_error_flag ses_write_pairs(ses_file_handle the_handle, ses_word_reference buf1, ses_word_reference buf2, ses_number dim) {
  

  /*  at the current file location, write a pair */

  ses_error_flag return_value = SES_NO_ERROR;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_write_pairs: File handle is not valid in ses_write_pairs\n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }

  struct _ses_setup* pSET = FILE_LIST[the_handle]->_the_setup;
  if (pSET->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_write_pairs:  setup not complete\n");
#endif
    return SES_SETUP_ERROR;
  }

  FILE* pFILE = _getPFILE(FILE_LIST[the_handle]->_the_handle);

  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_write_pairs: C file handle is null in ses_write_pairs\n");
#endif
    return SES_WRITE_ERROR;
  }


  ses_word_reference pairs = malloc(sizeof(ses_word)*2*dim);

  int i=0;  
  for(i=0; i< dim; i++) {
    pairs[2*i] = buf1[i];
    pairs[2*i+1] = buf2[i]; 
  }

  ses_error_flag write_error = SES_FALSE;
  write_error = ses_write_next(the_handle, pairs, dim, "no_label");


  free(pairs);
  pairs = NULL;



  return return_value;
}

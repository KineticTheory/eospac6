
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


#include <stdlib.h>
#include <string.h>

#undef DEBUG_PRINT

ses_boolean ses_set_ascii_word_type(ses_file_handle the_handle, ses_ascii_word_type the_type) {

  
  ses_boolean return_value = SES_FALSE;


  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_set_ascii_word_type: ses file handle not valid in ses_SET_ASCII_type\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return return_value;
  }

#ifdef DEBUG_PRINT
    printf("ses_set_ascii_word_type: ASCII_type: %c\n", the_type);
#endif
   
//    switch (the_type) {
//        case ASCII_MEDIUM_WORD_TYPE:
//            FILE_LIST[the_handle]->_the_handle->_word_size = ASCII_MEDIUM_WORD_SIZE;
//            return_value = SES_TRUE;
//           break;
//
//        case ASCII_SMALL_WORD_TYPE:
//            FILE_LIST[the_handle]->_the_handle->_word_size = ASCII_SMALL_WORD_SIZE;
//            return_value = SES_TRUE;
//            break;
//            
//        default:
//            FILE_LIST[the_handle]->_the_handle->_word_size = ASCII_MEDIUM_WORD_SIZE;
//            return_value = SES_TRUE;
//            break;
//    }

  if (the_type == ASCII_MEDIUM_WORD_TYPE) {
            FILE_LIST[the_handle]->_the_handle->_word_size = ASCII_MEDIUM_WORD_SIZE;
            return_value = SES_TRUE;
  }
  else {
	if (the_type == ASCII_SMALL_WORD_TYPE) {
            FILE_LIST[the_handle]->_the_handle->_word_size = ASCII_SMALL_WORD_SIZE;
            return_value = SES_TRUE;
        }
        else {
            FILE_LIST[the_handle]->_the_handle->_word_size = ASCII_MEDIUM_WORD_SIZE;
            return_value = SES_TRUE;
        }
  }  

   
  return return_value;
}


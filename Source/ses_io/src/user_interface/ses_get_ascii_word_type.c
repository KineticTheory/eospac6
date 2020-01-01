

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


#include <stdlib.h>
#include <string.h>



#undef DEBUG_PRINT

ses_ascii_word_type ses_get_ascii_word_type(ses_file_handle the_handle) {

  
    ses_ascii_word_type return_value = ASCII_MEDIUM_WORD_TYPE;
    int word_size = ASCII_MEDIUM_WORD_SIZE;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_GET_ASCII_word_type: ses file handle not valid in ses_GET_ASCII_type\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return return_value;
  }

    word_size = FILE_LIST[the_handle]->_the_handle->_word_size;
#ifdef DEBUG_PRINT
    printf("ses_GET_ASCII_word_type: word_size is %d\n", word_size);
#endif

//#define USE_CASE
#ifdef USE_CASE
    

	switch (word_size) {

       case ASCII_MEDIUM_WORD_SIZE:
            return_value = ASCII_MEDIUM_WORD_TYPE;
            break;
 
        case ASCII_SMALL_WORD_SIZE:
            return_value = ASCII_SMALL_WORD_TYPE;
            break;
            
        default:
            return_value = ASCII_MEDIUM_WORD_TYPE;
            FILE_LIST[the_handle]->_the_handle->_word_size = ASCII_MEDIUM_WORD_SIZE;
            break;
    }

#else

    if (word_size == ASCII_MEDIUM_WORD_SIZE) {
            return_value = ASCII_MEDIUM_WORD_TYPE;
    }
    else {
	if (word_size == ASCII_SMALL_WORD_SIZE) {
            return_value = ASCII_SMALL_WORD_TYPE;
	}
	else {
            return_value = ASCII_MEDIUM_WORD_TYPE;
            FILE_LIST[the_handle]->_the_handle->_word_size = ASCII_MEDIUM_WORD_SIZE;
	}
   }

#endif
    
    
  return return_value;
}


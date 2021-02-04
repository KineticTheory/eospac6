
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <stdarg.h>
#include <ctype.h>

#undef DEBUG_PRINT

/****
 *
 *
 *
 ****/
ses_error_flag ses_set_format(ses_number num_vars, ...) {
    
    int                 i;
    va_list             valist;
    ses_file_handle     the_handle;
    ses_file_type       the_file_type;
    ses_ascii_word_type the_ascii_word_type;
    
    ses_error_flag return_value = SES_NO_ERROR;
#ifdef DEBUG_PRINT
    printf ("ses_set_format: num_vars: %d\n", num_vars);
#endif
    va_start(valist, num_vars);
    the_ascii_word_type = 'M';

    if (num_vars < 2) {
        printf("There are not enough variables in ses_set_format. There nust be at list 2 beyond the # of vars.\n");
        return SES_INVALID_FILE_HANDLE;  // GINGER make a new error!
    }
    
    // Getting the parameters off of the list:
    for (i = 0; i < num_vars; i++) {
        
        switch (i) {
            case 0:
                the_handle = va_arg(valist, ses_file_handle);
#ifdef DEBUG_PRINT
                printf ("ses_set_format, ses_file_handle: %i\n", the_handle);
#endif
                break;
            case 1:
                the_file_type = (ses_file_type)va_arg(valist, int);
                the_file_type = toupper(the_file_type);
#ifdef DEBUG_PRINT
                printf ("ses_set_format, the_file_type: %c\n", the_file_type);
#endif
                // Verify that it's a valid file_type
                if ( (the_file_type != BINARY_TYPE) &&
                     (the_file_type != ASCII_TYPE)  &&
                     (the_file_type != XML_TYPE)    &&
                     (the_file_type != LLNL_TYPE)      ){
#ifdef DEBUG_PRINT
                    printf ("ses_set_format Error: the_file_type: %c\n", the_file_type);
#endif
                    return SES_INVALID_FILE_FORMAT_TYPE;
                }
                
                break;
            case 2:
                the_ascii_word_type = (ses_ascii_word_type)va_arg(valist, int);
                the_ascii_word_type = toupper(the_ascii_word_type);
#ifdef DEBUG_PRINT
                printf ("ses_set_format, ascii_word_type: %c\n", the_ascii_word_type);
#endif
                // Verify that it's a valid ascii_word_type
                if ( (the_ascii_word_type != ASCII_MEDIUM_WORD_TYPE) &&
                     (the_ascii_word_type != ASCII_SMALL_WORD_TYPE)    ){
                    return SES_INVALID_ASCII_WORD_TYPE;
                }
                break;
                
            default:
#ifdef DEBUG_PRINT
                printf ("ses_set_format, Invalid number of parameters: i = %d\n", i);
#endif
                return SES_INVALID_NUM_PARAMETERS;
                break;
        }
    }

    va_end(valist);

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_set_format: file handle invalid in ses_set_format \n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }

 
  struct _ses_file_handle* ptHandle = FILE_LIST[the_handle]->_the_handle;
  if (ptHandle == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_set_format: _ses_file_handle* NULL\n");
#endif
    return SES_NULL_OBJECT_ERROR;
  }

  /*  put the new file type to write on the file handle */

  ptHandle->_filetype = the_file_type; 

  /*  change the function pointers for the file handle */

  _set_format_type(ptHandle, the_file_type);

  /* if it's an ascii type, set the appropriate word side */
    if ( the_file_type == ASCII_TYPE ){
        if ( the_ascii_word_type == ASCII_MEDIUM_WORD_TYPE ){
            ptHandle->_word_size = ASCII_MEDIUM_WORD_SIZE;
        }
        else{
            ptHandle->_word_size = ASCII_SMALL_WORD_SIZE;
        }
    }
   
  if (ptHandle->_filetype == QUERY_4_TYPE) {
	return_value = SES_INVALID_OPEN_TYPE;
  }


  return return_value;

}


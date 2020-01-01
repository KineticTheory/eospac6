
////////////////  DEFINES ///////////////////////////
#define _SES_INTERNAL_
//  THIS DEFINE IS USED SO THAT #undef _SES_INTERNAL_ can be added when linking to C++ (John Groves example)
/////////////////////////////////////////////////////

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <string.h>

#define FIX_OPTIMIZATION_ERRORS

ses_boolean _construct_globals(ses_open_type open_flags) {
    
    /*  This function should be called upon first usage (ses_open) */
    /*  upon first usage, initialize _next_empty_file to 1
     the library does not use 0, because 0 means a NULL ses_file_handle */
    
    
    ses_boolean return_value = SES_TRUE;
    
    /*  create the registered formats list */
    
    _number_registered_formats = 4;
    _registered_formats = malloc(sizeof(ses_file_type) * 4);
    _registered_formats[0] = BINARY_TYPE;    //  default binary
    pt2_registered_functions = (ses_boolean (**)(FILE* ))malloc(sizeof(ses_boolean *) * 4);
    pt2_registered_functions[0] = &_isit_my_format_binary;
    _registered_formats[1] = ASCII_TYPE;     //  lanl sesame binary
    pt2_registered_functions[1] = &_isit_my_format_ascii;
    _registered_formats[2] = XML_TYPE;     //  lanl sesame xml
    pt2_registered_functions[2] = &_isit_my_format_xml;
    _registered_formats[3] = LLNL_TYPE;     //  llnl sesame xml
    pt2_registered_functions[3] = &_isit_my_format_llnl_xml;
    
    /*  set the default current format */
    
    _current_format = BINARY_TYPE;
    _default_format = BINARY_TYPE;
#ifdef FIX_OPTIMIZATION_ERRORS
    _set_current_format(BINARY_TYPE);
#else
    _set_current_format(_current_format);
#endif
    pt2_isit_my_format = &_isit_my_format_binary;
    
    if ((open_flags != 'A') && (open_flags != 'C') &&
        (open_flags != 'W') && (open_flags != 'R')) {
#ifdef DEBUG_PRINT
        printf("_construct_globals:  found invalid file type %c\n", open_flags);
#endif
        _set_latest_error(SES_INVALID_OPEN_TYPE);
        return SES_FALSE;
    }
    
    if (_next_empty_file == 0) {
        /*  because a value of 0 would mean a NULL ses_file_handle */
        _next_empty_file = 1;
    }
    
    /*  construct the standard tables */
    
    _latest_error = SES_NO_ERROR;
    
    return_value = _construct_standard_tables();
    if (return_value == SES_FALSE) {
#ifdef DEBUG_PRINT
        printf("_construct_globals: standard tables failed to construct in _construct_globals\n");
#endif
        _set_latest_error(SES_FUNCTION_FAIL_ERROR);
        return SES_FALSE;
    }

    _number_open_handles = 0;
    
    return return_value;
    
}

/***************************************************************/

int _set_latest_error(const ses_error_flag the_flag) {
    
    /*  set the latest error */
    
    int return_value = 0;
    
    if ((the_flag < -5) || (the_flag > 100)) {
#ifdef DEBUG_PRINT
        printf("_set_latest_error: invalid ses_error_flag = %d\n", the_flag);
#endif
        return_value = -1;
        return return_value;
    }
    
    _latest_error = the_flag;
    return return_value;
    
}

/***************************************************************/

ses_boolean      _globals_contains(ses_string filename){
    
    //  return SES_TRUE if the FILE_LIST array contains the filename
    //  return SES_FALSE otherwise
    
    ses_boolean return_value = SES_FALSE;
    
    int i = 0;
    struct _ses_file_handle* pSFH = (struct _ses_file_handle*)NULL;
    for (i = 1; i < _next_empty_file; i++) {
        if (FILE_LIST[i] != (struct _ses_file*)NULL) {
	        pSFH = FILE_LIST[i]->_the_handle;
	        if (pSFH != (struct _ses_file_handle*)NULL) {
	            
	            if (pSFH->_filename != (ses_string)NULL) {
	                if (strcmp(pSFH->_filename, filename) == 0) {
	                    return_value = SES_TRUE;
	                }
	            }
	        }
	}        
        
    }
    
    return return_value;
}

/***************************************************************/

ses_file_handle  _get_handle_from_globals(ses_string filename){
    
    //  returns the file handle associated with the filename
    
    ses_file_handle return_value = -1;
    
    int i = 0;
    struct _ses_file_handle* pSFH = (struct _ses_file_handle*)NULL;
    
    for (i = 1; i < _next_empty_file; i++) {
        
       if (FILE_LIST[i] != (struct _ses_file*)NULL) {
            pSFH = FILE_LIST[i]->_the_handle;
            if (pSFH != (struct _ses_file_handle*)NULL) {
            	if (pSFH->_filename != (ses_string)NULL) {
                
            	    if (strcmp(pSFH->_filename, filename) == 0) {
            	        return_value = i;
            	    }
            	}
            }
       }
        
    }
    
    
    return return_value;
}

/***************************************************************/

int _get_next_empty_slot_from_globals(void) {
    
    /*  get the next empty slot in the global file list array*/
    
    ses_file_handle return_value = _next_empty_file;
    
    int i = 0;
    struct _ses_file_handle* pSFH = (struct _ses_file_handle*)NULL;
    for (i = 1; i < _next_empty_file; i++) {
        
      if (FILE_LIST[i] != (struct _ses_file*)NULL) {
         pSFH = FILE_LIST[i]->_the_handle;
         if (pSFH == (struct _ses_file_handle*)NULL) {
            return_value = i;
            break;
         }
      }
        
        
        
    }
    
    if (return_value == _next_empty_file) {
        
        _next_empty_file++;
        
    }
    
    
    return return_value;
    
    
}

/***************************************************************/

ses_boolean _destruct_globals(void) {
    
    /*  destruct global data */
    
    ses_boolean return_value = SES_TRUE;
    
    return_value = _destruct_standard_tables();
    if (return_value == SES_FALSE) {
#ifdef DEBUG_PRINT
        printf("_destruct_globals: standard tables failed to destruct in _destruct_globals\n");
#endif
        _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
    }
    
    NUMBER_TABLES = 0;
    
    ses_boolean file_list_destruct = _destruct_file_list();
    return_value = return_value && file_list_destruct;
    if (file_list_destruct == SES_FALSE) {
#ifdef DEBUG_PRINT
        printf("_destruct_globals: file_list failed to destruct in _destruct_globals\n");
#endif
        _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
    }
    
    _next_empty_file = 0;
    _latest_error = SES_NO_ERROR;
    
    
    free(_registered_formats);
    _registered_formats = (ses_file_type*)NULL;
    free(pt2_registered_functions);
    pt2_registered_functions = (ses_boolean (**)(FILE* ))NULL;
    
    
    _number_registered_formats = 0;
    _current_format = UNKNOWN_TYPE;
    pt2_isit_my_format = NULL;
    
    
    return return_value;
}
/***************************************************************/

ses_boolean _destruct_file_list(void) {
    
    /*  destruct the global file list */
    
    ses_boolean return_value = SES_TRUE;
    
    int i = 0;
    for (i = 1; i < _next_empty_file; i++) {
        if (FILE_LIST[i] != (struct _ses_file*)NULL) {
            ses_boolean didit_destruct = _destruct_ses_file(FILE_LIST[i]);
            if (didit_destruct == SES_FALSE) {
#ifdef DEBUG_PRINT
                printf("_destruct_file_list: ses_file at handle %d failed to destruct\n", i);
#endif
                _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
            }
            free(FILE_LIST[i]);
            FILE_LIST[i] = (struct _ses_file*)NULL;
            return_value = return_value && didit_destruct;
        }
    } 
    
    return return_value;
    
}

/***************************************************************/

void _set_current_format(ses_file_type the_type) {
    
    /*  set the current default file format */
    
    _current_format = UNKNOWN_TYPE;
    int i = 0;
    for (i = 0; i < _number_registered_formats; i++) {
        if (_registered_formats[i] == the_type) {
            _current_format = the_type;
            pt2_isit_my_format = pt2_registered_functions[i];
        }
    }
    
}


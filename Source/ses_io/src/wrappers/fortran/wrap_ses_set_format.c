

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
    ses_error_flag ses_set_format_(ses_number* pt_num_args, ses_file_handle* pt_handle, ses_file_type* pt_type, ses_ascii_word_type* pt_ascii_type) {
#else
    
#ifdef UC_UNDER
    ses_error_flag SES_SET_FORMAT_(ses_number* pt_num_args, ses_file_handle* pt_handle, ses_file_type* pt_type, ses_ascii_word_type* pt_ascii_type) {
#else
        
#ifdef LC_NOUNDER
    ses_error_flag ses_set_format(ses_number* pt_num_args, ses_file_handle* pt_handle, ses_file_type* pt_type, ses_ascii_word_type* pt_ascii_type) {
#else
            
#ifdef UC_NOUNDER
    ses_error_flag SES_SET_FORMAT(ses_number* pt_num_args, ses_file_handle* pt_handle, ses_file_type* pt_type, ses_ascii_word_type* pt_ascii_type) {
#endif
                
#endif
#endif
#endif
                
    ses_error_flag return_value = SES_NO_ERROR;
                
    ses_number          num_args   = *pt_num_args;
    ses_file_handle     the_handle = *pt_handle;
    ses_file_type       the_type   = *pt_type;
    ses_ascii_word_type ascii_type = *pt_ascii_type;

#ifdef DEBUG_WRAP
    printf("wrap_ses_set_format.c:  num_args is %d\n", num_args);
    printf("wrap_ses_set_format.c:  the_handle is %d\n", the_handle);
    printf("wrap_ses_set_format.c:  the format is %c\n", the_type);
    printf("wrap_ses_set_format.c:  the ascii type is %c\n", ascii_type);
#endif
                
    return_value = ses_set_format(num_args, the_handle, the_type, ascii_type);
                
#ifdef DEBUG_WRAP
    printf("wrap_ses_set_format.c:  return_value is %d\n", return_value);
#endif

    return return_value;
                
}
            
            

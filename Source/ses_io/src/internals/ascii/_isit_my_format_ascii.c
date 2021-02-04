#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "_file_list_ascii.h"

#undef DEBUG_PRINT
ses_boolean _isit_my_format_ascii(FILE* pFILE) {
    
    ses_boolean return_value = SES_FALSE;
    if (pFILE != (FILE*)NULL) {
        
        rewind(pFILE);
        
        ses_boolean needs_flip = SES_FALSE;
        
        long nfiles = 0;
        nfiles = _read_long_tomax_pFILE_ascii(pFILE, 2, needs_flip);
#ifdef DEBUG_PRINT
        printf("_isit_my_format_ascii:  nfiles is %ld\n", nfiles);
#endif
        if (nfiles == 0) {
            
            //  read the next long, which should be the Material ID with 6 characters:
            
            long dummy = _read_long_tomax_pFILE_ascii(pFILE, 6, needs_flip);
#ifdef DEBUG_PRINT
            printf("_isit_my_format_ascii:  dummy is %ld\n", dummy);
#endif
            
            if (dummy != 0) {
                return_value = SES_TRUE;
            }
            
            
        }
        rewind(pFILE);
        
    }
    
    return return_value;
}

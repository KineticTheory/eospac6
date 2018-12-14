
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "../xml_utilities.h"

#define check_errors_READ_LONG_LLNL_XML HEADER(check_errors_READ_LONG_LLNL_XML)

long _read_long_llnl_xml(struct _ses_file_handle* pSFH) {

  /*  function prototypes */

   ses_error_flag check_errors_READ_LONG_LLNL_XML(struct _ses_file_handle* pSFH);

   /*  end function prototypes */

   ses_error_flag return_flag = check_errors_READ_LONG_LLNL_XML( pSFH);
   if (return_flag != SES_NO_ERROR) {
	return 0;
   }

   long return_value = 0;
   double the_double = _read_double_pFILE_xml(pSFH->_c_file_handle);
   return_value =  (long)the_double;
        
   return return_value;
}

ses_error_flag check_errors_READ_LONG_LLNL_XML(struct _ses_file_handle* pSFH) {

   if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
      printf("_read_long_llnl_xml: ses file handle null\n");
#endif
      return SES_NULL_OBJECT_ERROR;
   }
   return SES_NO_ERROR;
}


ses_boolean        _write_long_llnl_xml(struct _ses_file_handle* pSFH, long the_long){
  return SES_FALSE;
}



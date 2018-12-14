#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "../xml_utilities.h"
#include <string.h>

#define check_errors_ISIT_MY_FORMAT_LLNL_XML HEADER(check_errors_ISIT_MY_FORMAT_LLNL_XML)

ses_boolean _isit_my_format_llnl_xml(FILE* pFILE) {

  /*  function prototypes */

   ses_error_flag check_errors_ISIT_MY_FORMAT_LLNL_XML(FILE* pFILE);

   /*  end function prototypes */

   ses_error_flag return_flag = check_errors_ISIT_MY_FORMAT_LLNL_XML(pFILE);
   if (return_flag != SES_NO_ERROR) {
	return SES_FALSE;
   }

  ses_boolean return_value = SES_FALSE;

  /*  Determine whether the file is XML 'L' */

  /*  if the file is LLNL XML, it will contain the string "<library>"  as the first non ?xml tag */

   rewind(pFILE);

   char* the_tag = _skip_tag(pFILE, "<?xml");
   if (the_tag != (char*)NULL) {
	free(the_tag);
	the_tag = (char*)NULL;
   }

   the_tag = _skip_tag(pFILE, "<library");
   if (the_tag != (char*)NULL) {
        if (strstr(the_tag, "<library") != NULL) {
           return_value = SES_TRUE;
        }
        free(the_tag);
        the_tag = (char*)NULL;
   }

   rewind(pFILE);

   return return_value;
}
ses_error_flag check_errors_ISIT_MY_FORMAT_LLNL_XML(FILE* pFILE) {

   if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
      printf("_isit_my_format_llnl_xml: c file handle null \n");
#endif
      return SES_NULL_OBJECT_ERROR;
   } 
   return SES_NO_ERROR;
}


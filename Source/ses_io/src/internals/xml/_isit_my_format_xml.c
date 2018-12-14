#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "../xml_utilities.h"
#include <string.h>


ses_boolean _isit_my_format_xml(FILE* pFILE) {

  ses_boolean return_value = SES_FALSE;

  /*  Determine whether the file is XML 'X' */

  /*  if the file is SESAME XML, it will contain the string "<directory>"  at the beginning of the file */

   rewind(pFILE);

   char* the_tag = _skip_tag(pFILE, "<directory>");
   if (the_tag != (char*)NULL) {
       if (strstr(the_tag, "<directory>") != NULL) {
          return_value = SES_TRUE;
          free(the_tag);
          the_tag = (char*)NULL;
       }
       else {
	   free(the_tag);
	   the_tag = (char*)NULL;
       }
   }

   rewind(pFILE);

   return return_value;
}

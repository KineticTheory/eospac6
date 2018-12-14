
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "_file_list_binary.h"

#define _isit_my_format_binary HEADER(_isit_my_format_binary)

ses_boolean _isit_my_format_binary(FILE* pFILE) {

	ses_boolean return_value = SES_FALSE;
	if (pFILE != (FILE*)NULL) {

		  rewind(pFILE);

		  ses_boolean needs_flip = SES_FALSE;
		  long nfiles = _read_long_pFILE_binary(pFILE, needs_flip);
		  if (nfiles <= 0) {

 		     needs_flip = SES_TRUE;
		     rewind(pFILE);
		     nfiles = _read_long_pFILE_binary(pFILE, needs_flip);

                     /*  if nfiles <= 0 here, you have some other file type */

		    if (nfiles > 0) { 
		      return SES_TRUE;
		    }


		  }
		  else {
			return_value = SES_TRUE;
		  }
		  rewind(pFILE);

	}

	return return_value;
}

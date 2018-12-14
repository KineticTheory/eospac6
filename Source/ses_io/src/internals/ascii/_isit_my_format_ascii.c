#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "_file_list_ascii.h"


ses_boolean _isit_my_format_ascii(FILE* pFILE) {


	ses_boolean return_value = SES_FALSE;
	if (pFILE != (FILE*)NULL) {

		  rewind(pFILE);

		  ses_boolean needs_flip = SES_FALSE;

		  long nfiles = 0;
		  nfiles = _read_long_pFILE_ascii(pFILE, needs_flip);
		  long matid = 0;
		  matid = _read_long_pFILE_ascii(pFILE, needs_flip);

		  if (matid > 0) {
			return_value = SES_TRUE;
		  }
		  rewind(pFILE);

	}

	return return_value;
}

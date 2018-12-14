

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"

#include "_file_list_ascii.h"


ses_boolean _write_directory_ascii(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH) {

  /*  write the directory to the c file handle */	
	
  ses_boolean return_value = SES_TRUE;


  /****************************************************************************/
  /*  argument error checking */

  if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_directory_ascii: null directory passed to _write_directory\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_directory_ascii: null ses file handle passed into _write_directory\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (_isit_ready_directory(ptDIR) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_write_directory_ascii: Trying to write the directory and it is not ready \n");
#endif
    _set_latest_error(SES_OBJECT_READY_ERROR);
    return SES_FALSE;
  }

  if (ptDIR->_nfiles <= 0) {
#ifdef DEBUG_PRINT
    printf("_write_directory_ascii: _nfiles <= 0 in _write_directory \n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_FALSE;
  }
  /****************************************************************************/

  /*  here we're good to go */



  /* return */

  return return_value;
}









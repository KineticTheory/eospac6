


#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"


ses_boolean _write_directory_binary(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH) {


  /*  write the directory to the c file handle */	
	
  ses_boolean return_value = SES_TRUE;

  /****************************************************************************/
  /*  argument error checking */

  if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_directory_binary: null directory passed to _write_directory\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_directory_binary: null ses file handle passed into _write_directory\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (_isit_ready_directory(ptDIR) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_write_directory_binary: Trying to write the directory and it is not ready \n");
#endif
    _set_latest_error(SES_OBJECT_READY_ERROR);
    return SES_FALSE;
  }

  if (ptDIR->_nfiles <= 0) {
#ifdef DEBUG_PRINT
    printf("_write_directory_binary: _nfiles <= 0 in _write_directory \n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_FALSE;
  }
  /****************************************************************************/

  /*  here we're good to go */

  _write_long(pSFH, ptDIR->_nfiles);

  int i=0;
  
  _write_long(pSFH, ptDIR->_date);
  _write_long(pSFH, ptDIR->_version);


  for (i=0; i < ptDIR->_nfiles; i++) {
    _write_long(pSFH, ptDIR->_matid[i]);
  }

 
  for (i=0; i < ptDIR->_nfiles; i++) {
    _write_long(pSFH, ptDIR->_nwds[i]);
  }

 
  for (i=0; i < ptDIR->_nfiles; i++) {
    _write_long(pSFH, ptDIR->_iadr[i]);
  }

  /* return */

  return return_value;
}









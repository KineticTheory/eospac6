#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#define _get_directory_size_llnl_xml HEADER(_get_directory_size_llnl_xml)
#define check_errors_gdslx HEADER(check_errors_gdslx)


long _get_directory_size_llnl_xml(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH) {

  //  function prototypes

  ses_error_flag check_errors_gdslx(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH);

  //  end function prototypes


  long return_value = 0;

  ses_error_flag error_check = check_errors_gdslx(ptDIR, pSFH);
  if (error_check != SES_NO_ERROR) {
	return return_value;
  }

  /*  return the size of the directory */

  return_value = 3 + 3*ptDIR->_nfiles;

  return return_value;

}

ses_error_flag check_errors_gdslx(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH) {

  /*  argument error checking */

  if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_directory_size: null directory pointer\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (ptDIR->_ready == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_directory_size: Trying to _get_directory_size from directory that is not ready\n");
#endif
    _set_latest_error(SES_OBJECT_READY_ERROR);
    return SES_OBJECT_READY_ERROR;
  }
  

  /*  compute the size */

  if (ptDIR->_nfiles <= 0) {
#ifdef DEBUG_PRINT
    printf("_get_directory_size: nfiles <= 0 in _get_directory_size\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }

  return SES_NO_ERROR;
}


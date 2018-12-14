#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


/*-----------------------------------------------*/
long _get_directory_size_ascii(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH) {

  /*  return the size of the directory */

  long return_value = 0;

  /*******************************************************************/
  /*  argument error checking */

  if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_directory_size: null directory pointer\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  if (ptDIR->_ready == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_directory_size: Trying to _get_directory_size from directory that is not ready\n");
#endif
    _set_latest_error(SES_OBJECT_READY_ERROR);
    return 0;
  }
  /*****************************************************************/  

  /*  compute the size */

  if (ptDIR->_nfiles <= 0) {
#ifdef DEBUG_PRINT
    printf("_get_directory_size: nfiles <= 0 in _get_directory_size\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return 0;
  }


  return_value = 3 + 3*ptDIR->_nfiles;


  /*  return */

  return return_value;

}

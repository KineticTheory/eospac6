#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_boolean  _go_to_next_array_location_binary(struct _ses_file_handle* pSFH, long location) {

  /********************************************************************/
  /*  error check the arguments */
#ifdef DEBUG_GO_TO_NEXT_ARRAY_LOCATION
   printf("_go_to_next_array_location_binary:  entered\n");
#endif

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_go_to_next_array_location_binary: Null ses file handle passed in\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  FILE* pFILE = pSFH->_c_file_handle;
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_go_to_next_array_location_binary: Null FILE* \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (location < 0) {
#ifdef DEBUG_PRINT
    printf("_go_to_next_array_location_binary:  invalid address\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_FALSE;
  }

  /********************************************************************/

  /*  go to the next array */

  int fseek_return = fseek(pFILE, location, SEEK_SET);
  if (fseek_return != 0) {
#ifdef DEBUG_PRINT
    printf("_go_to_next_array_location_binary: fseek return is bad\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_FALSE;
  }


  return SES_TRUE;
}



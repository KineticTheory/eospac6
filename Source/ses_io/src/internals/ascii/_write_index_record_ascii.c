#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


#include "stdio.h"
#include "string.h"

#include "_file_list_ascii.h"


ses_boolean _write_index_record_ascii(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH) {


  /*  write an index record to the c file handle */

  ses_boolean return_value = SES_TRUE;

  /****************************************************************/
  /*  error check the arguments */

  if (ptIR == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_index_record_ascii: null index record pointer passed to write index record\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_index_record_ascii: null ses file handle passed to _write_index_record\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }
  /****************************************************************/


  pSFH->_start_index = 0;



  return return_value;
}


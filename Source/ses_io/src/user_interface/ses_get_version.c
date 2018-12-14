
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


#include <stdlib.h>
#include <string.h>


long  ses_get_version(ses_file_handle the_handle) {

  
  long return_value = (long)NULL;


  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_GET_VERSION: ses file handle not valid in SES_GET_VERSION\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return return_value;
  }

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;

  FILE* pFILE = 0;
  pFILE = _getPFILE(pSFH);

  struct _ses_directory* ptDIR = FILE_LIST[the_handle]->_directory;

  ses_boolean read_dir = SES_FALSE;
  if (ptDIR == (struct _ses_directory*)NULL) {

    ptDIR  = _read_directory(pSFH);
    read_dir = SES_TRUE;
    if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
      printf("SES_GET_VERSION: did not read directory \n");
#endif
      _set_latest_error(SES_READ_ERROR);
      return (long)NULL;
    }
  }
  return_value = ptDIR->_version;

  _releasePFILE(pSFH);


  return return_value;
}



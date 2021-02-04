
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


#include <stdlib.h>
#include <string.h>

//#define DEBUG_SES_GET_DATE

long  ses_get_date(ses_file_handle the_handle) {

  
  long return_value = (long)NULL;

#ifdef DEBUG_SES_GET_DATE
  printf("ses_get_date:  the_handle is %d\n", the_handle);
#endif


  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_get_date: ses file handle not valid in SES_GET_DATE\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return return_value;
  }

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;

  /* FILE* pFILE = 0; */
  /* pFILE = */ _getPFILE(pSFH);

  struct _ses_directory* ptDIR = FILE_LIST[the_handle]->_directory;
#ifdef DEBUG_SES_GET_DATE
  printf("ses_get_date:  ptDIR is %ld\n", ptDIR);
#endif

  /* ses_boolean read_dir = SES_FALSE; */
  if (ptDIR == (struct _ses_directory*)NULL) {

    ptDIR  = _read_directory(pSFH);
    /* read_dir = SES_TRUE; */
    if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
      printf("SES_GET_DATE: did not read directory \n");
#endif
      _set_latest_error(SES_READ_ERROR);
      return (long)NULL;
    }
  }
#ifdef DEBUG_SES_GET_DATE
  printf("ses_get_date:  ptDIR->_date  is %d\n", ptDIR->_date);
#endif
  return_value = ptDIR->_date;
#ifdef DEBUG_SES_GET_DATE
  printf("ses_get_date:  return_value is %ld\n", return_value);
#endif

  _releasePFILE(pSFH);


  return return_value;
}



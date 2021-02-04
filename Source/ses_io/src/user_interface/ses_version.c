
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"
#include <string.h>

ses_string ses_version(ses_file_handle the_handle) {
  
  /*  function prototypes */

  ses_string _make_into_string(long the_long);

  /*  end function prototypes */
 
  ses_string return_value = (ses_string)NULL;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_version: file handle not valid in ses_version\n");
#endif
    return 0;
  }

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_version: ses_file_handle null in ses_version\n");
#endif
    return 0;
  }

  /* FILE* pFILE = 0; */
  /* pFILE = */ _getPFILE(pSFH);

  struct _ses_directory* ptDIR = FILE_LIST[the_handle]->_directory;
  ses_boolean didit_read = SES_FALSE;
  if (ptDIR == (struct _ses_directory*)NULL) {


    ptDIR  = _read_directory(pSFH);
    didit_read = SES_TRUE;
    if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
      printf("ses_version: did not read directory \n");
#endif
      _set_latest_error(SES_READ_ERROR);
      return (ses_string)NULL;
    }
  }

    
    return_value = malloc(sizeof(char)*SES_MAX_STRING_SIZE);
    ses_string version_string = _make_into_string(ptDIR->_version);
    strcpy(return_value, version_string);

    free(version_string);
    version_string = (ses_string)NULL;

    if (didit_read == SES_TRUE) {
      _destruct_ses_directory(ptDIR);
      free(ptDIR);
      FILE_LIST[the_handle]->_directory = (struct _ses_directory*)NULL;
      ptDIR = (struct _ses_directory*)NULL;
    }
 
  



  _releasePFILE(pSFH);


  return return_value;
}

ses_string _make_into_string(long the_long) {
  /*  make  a long into a string */

 
  void itoa(long n, char s[]);

  ses_string return_value = malloc(sizeof(char)*8);
  itoa(the_long, return_value);



  return return_value;

}

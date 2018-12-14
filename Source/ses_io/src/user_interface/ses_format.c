
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


#include <stdlib.h>
#include <string.h>


ses_string ses_format(ses_file_handle the_handle) {


  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_format: ses file handle not valid in ses_format\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return NULL_STRING;
  }

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;

  char the_format = pSFH->_filetype;

  /*  everythings ready, make the string */

  ses_string return_value = malloc(sizeof(char) * 2);
  return_value[0] = the_format;
  return_value[1] = '\0';


  return return_value;
}





#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_boolean ses_is_valid (ses_file_handle the_handle) {

  /*  this function returns false (SES_FALSE) if the ses_file_handle is 0 */
  /*  this function returns true  (SES_TRUE) if the ses_file_handle is >0 and
      less than _next_empty_file */

  ses_boolean return_value = SES_FALSE;

//#define DEBUG_IS_VALID
#ifdef DEBUG_IS_VALID
  printf ("ses_is_valid:  the_handle is %d\n", the_handle);
#endif

  if ((the_handle > 0) && FILE_LIST[the_handle] != (struct _ses_file*)NULL) {
  	struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;

  	if ((the_handle > 0) && (the_handle < _next_empty_file) && ( pSFH != (struct _ses_file_handle*)NULL) ) {
  	  return_value = SES_TRUE;
  	}

  }

  return return_value;
}

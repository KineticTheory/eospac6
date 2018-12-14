
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

ses_boolean ses_indicates_error(ses_error_flag the_ses_error_flag) {

  ses_boolean return_value = SES_FALSE;

  if (the_ses_error_flag != SES_NO_ERROR) {
    return_value = SES_TRUE;
  }

  return return_value;
}

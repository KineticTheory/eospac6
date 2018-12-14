
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_boolean ses_exit_(void) {
#else

#ifdef UC_UNDER
ses_boolean SES_EXIT_(void) {
#else

#ifdef LC_NOUNDER
ses_boolean ses_exit(void) {
#else

#ifdef UC_NOUNDER
ses_bolean SES_EXIT(void) {
#endif

#endif
#endif
#endif

  ses_boolean return_value = SES_FALSE;

  return_value = ses_exit();

  return return_value;

}



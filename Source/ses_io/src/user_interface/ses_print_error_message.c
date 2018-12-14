
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <string.h>


ses_string ses_print_error_message(ses_error_flag the_error_flag) {

  /*  function prototype */

  ses_string _get_error(ses_error_flag the_error_flag);

  /*  end function prototypes */

  ses_string return_value = _get_error(the_error_flag);

  return return_value;
}


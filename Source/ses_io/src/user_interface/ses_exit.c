
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_boolean ses_exit(void) {
	return _destruct_globals();

}


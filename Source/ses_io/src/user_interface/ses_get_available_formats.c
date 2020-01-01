

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_string ses_get_available_formats(void) {


	if (_number_registered_formats == 0) {
		/* ses_boolean didit_construct = SES_FALSE; */
		/* didit_construct = */ _construct_globals('R');
	}

	ses_string return_value = malloc(sizeof(char)* (_number_registered_formats + 1));

	int i = 0;
	for (i = 0; i < _number_registered_formats; i++) {
		return_value[i] = _registered_formats[i];
	}
	return_value[_number_registered_formats] = '\n';

	return return_value;
}


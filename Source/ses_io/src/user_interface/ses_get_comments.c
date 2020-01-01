
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <string.h>

#include <stdio.h>
#include <stdlib.h>

#undef DEBUG_SES_GET_COMMENTS
#undef DEBUG_PRINT

ses_error_flag ses_get_comments(ses_file_handle the_handle, ses_string* the_string) {

  /*  returns all the comments on ANY comment table (101-199) */



  ses_error_flag return_value = SES_NO_ERROR;
  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_get_comments: ses file handle not valid in ses_comments\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_INVALID_FILE_HANDLE;
  }

  /*  get the current setup information, so you can return to that state */

  ses_material_id current_material = FILE_LIST[the_handle]->_the_setup->_mid;
  ses_table_id current_tid = FILE_LIST[the_handle]->_the_setup->_tid;

  int i = 0;
  int j = 0;
  ses_error_flag didit_setup = SES_NO_ERROR;
  /* ses_error_flag didit_get_comments = SES_NO_ERROR; */
  ses_string the_new_string = malloc(sizeof(char) * 1);   /*  start with '\0' */

  strcpy(the_new_string, "");
  int string_size = strlen(the_new_string);
  int old_string_size = 0;
  for (i = 101; i <= 199; i++) {

	/*  setup to current_material, tid = i */

	didit_setup = ses_setup(the_handle, current_material, (ses_table_id)i);
	ses_string* local_string = malloc(sizeof(ses_string) * 1);
	if (didit_setup == SES_NO_ERROR) {

		/*  get the comments from that table */

		local_string[0] = (ses_string)NULL;
		/* didit_get_comments = */ ses_comments(the_handle, local_string);
#ifdef DEBUG_SES_GET_COMMENTS
		printf("for table %d local_string is %s\n", i, local_string[0]);
#endif

		if (string_size > 0) {
			the_new_string[string_size-1] = ' ';
		}
		else {
			the_new_string[0] = ' ';
		}
		string_size = string_size + strlen(local_string[0]) + 1 + 2;  //  1 for \0 2 for \n\n
                the_new_string = (char*)realloc(the_new_string, sizeof(char)*string_size);
		if (the_new_string != (ses_string)NULL) {
			for (j=old_string_size; j < string_size; j++) {
				the_new_string[j] = ' ';
			}
			the_new_string[old_string_size] = '\0';
			old_string_size = string_size-1;
			strcat(the_new_string, local_string[0]);
			strcat(the_new_string, "\n\n");
		}

		free(local_string[0]);
		local_string[0] = (ses_string)NULL;
	}
	free(local_string);
	local_string = (ses_string*)NULL;

  }

  /*  setup to the original value */

  didit_setup = ses_setup(the_handle, current_material, current_tid);

  *the_string = the_new_string;

#ifdef DEBUG_SES_GET_COMMENTS
  printf("*the_string is %s\n", *the_string);
#endif

  return return_value;

}


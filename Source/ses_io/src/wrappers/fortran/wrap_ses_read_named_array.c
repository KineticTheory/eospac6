

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>
#include <string.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_error_flag ses_read_named_array_(ses_file_handle* pt_handle, ses_label the_label, ses_word_reference buf) {
#else

#ifdef UC_UNDER
ses_error_flag SES_READ_NAMED_ARRAY_(ses_file_handle* pt_handle, ses_label the_label, ses_word_reference buf) {
#else

#ifdef LC_NOUNDER
ses_error_flag ses_read_named_array(ses_file_handle* pt_handle, ses_label the_label, ses_word_reference buf) {
#else

#ifdef UC_NOUNDER
ses_error_flag SES_READ_NAMED_ARRAY(ses_file_handle* pt_handle, ses_label the_label, ses_word_reference buf) {
#endif

#endif
#endif
#endif

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle the_handle = *pt_handle;
#ifdef DEBUG_WRAP
  printf("wrap_ses_read_named_array.c:  the_handle is %d\n", the_handle);
  printf("wrap_ses_read_named_array.c:  the_label is %s\n", the_label);
  
#endif
  int new_len = strlen(the_label);
  char new_string[new_len];
  strcpy(new_string, the_label);
  new_string[new_len-1] = '\0';
#ifdef DEBUG_WRAP
  printf("wrap_ses_read_named_array.c:  new_string is %s\n", new_string);
#endif
  return_value = ses_read_named_array(the_handle, &new_string[0], buf);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_read_named_array.c:  return_value is %d\n", return_value);
  printf("wrap_ses_read_named_array.c:  buf[0] is %e\n", buf[0]);
#endif
  return return_value;

}

 

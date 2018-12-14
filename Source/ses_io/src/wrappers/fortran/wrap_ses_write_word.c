

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_error_flag ses_write_word_(ses_file_handle* pt_handle, ses_word_reference buffer) {
#else

#ifdef UC_UNDER
ses_error_flag SES_WRITE_word_(ses_file_handle* pt_handle, ses_word_reference buffer) {
#else

#ifdef LC_NOUNDER
ses_error_flag ses_write_word(ses_file_handle* pt_handle, ses_word_reference buffer) {
#else

#ifdef UC_NOUNDER
ses_error_flag SES_WRITE_word(ses_file_handle* pt_handle, ses_word_reference buffer) {
#endif

#endif
#endif
#endif

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle the_handle = *pt_handle;
  ses_word the_word = *buffer;
#ifdef DEBUG_WRAP
  printf("wrap_ses_write_word.c:  the_handle is %d\n", the_handle);
  printf("wrap_ses_write_word.c:  the_word is %e\n", the_word);
#endif

  return_value = ses_write_word(the_handle, the_word);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_write_word.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 

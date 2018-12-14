
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

//ses_string ses_print_error_message(ses_error_flag the_error_flag) {

#ifdef LC_UNDER
ses_boolean ses_print_error_message_(ses_error_flag* the_error_flag, ses_string the_buffer, int* dim) {
#else

#ifdef UC_UNDER
ses_boolean SES_PRINT_ERROR_MESSAGE_(ses_error_flag* the_error_flag, ses_string the_buffer, int* dim) {
#else

#ifdef LC_NOUNDER
ses_boolean ses_print_error_message(ses_error_flag* the_error_flag, ses_string the_buffer, int* dim) {
#else

#ifdef UC_NOUNDER
ses_boolean SES_PRINT_ERROR_MESSAGE(ses_error_flag* the_error_flag, ses_string the_buffer, int* dim) {
#endif

#endif
#endif
#endif

  ses_boolean return_value = SES_TRUE;

  ses_error_flag my_flag = *the_error_flag;


  ses_string my_string = ses_print_error_message(my_flag);
#ifdef DEBUG_WRAP
  printf("wrap_ses_print_error_message.c:  the-error_flag is %d my_string is %s\n", my_flag, my_string);
#endif

  int i = 0;
  for (i = 0; i < strlen(my_string); i++) {
	the_buffer[i] = my_string[i];
  }
  the_buffer[i] = '\0';
#ifdef DEBUG_WRAP
  printf("wrap_ses_print_error_message.c:  the_buffer is %s\n", the_buffer);
#endif

  *dim = strlen(my_string);

  return return_value;

}

 



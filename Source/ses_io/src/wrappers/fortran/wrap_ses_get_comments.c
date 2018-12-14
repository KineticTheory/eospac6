

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

//#define DEBUG_WRAP

#ifdef LC_UNDER
ses_error_flag ses_get_comments_(ses_file_handle* pt_handle, ses_string the_string, ses_number* the_size) {
#else

#ifdef UC_UNDER
ses_error_flag SES_GET_COMMENTS_(ses_file_handle* pt_handle, ses_string the_string, ses_number* the_size) {
#else

#ifdef LC_NOUNDER
ses_error_flag ses_get_comments(ses_file_handle* pt_handle, ses_string the_string, ses_number* the_size) {
#else

#ifdef UC_NOUNDER
ses_error_flag SES_GET_COMMENTS(ses_file_handle* pt_handle, ses_string the_string, ses_number* the_size) {
#endif

#endif
#endif
#endif


  int size_of_string(ses_string str);

  ses_error_flag return_value = SES_NO_ERROR;
  ses_file_handle the_handle = *pt_handle;

#ifdef DEBUG_WRAP
  printf("wrap_ses_get_comments.c:  the_handle is %d\n", *pt_handle);
#endif

  ses_string almost_return_value2 = 0;

  return_value = ses_get_comments(the_handle, &almost_return_value2);
#ifdef DEBUG_WRAP
  printf("wrap_ses_get_comments.c: almost_return_value2 is %s\n", almost_return_value2);
#endif
  int the_number  = size_of_string(almost_return_value2);
#ifdef DEBUG_WRAP
  printf("wrap_ses_get_comments.c:  size_of_string is %d\n", the_number);
#endif
  
  int i = 0;
  for (i=0; i < the_number; i++) {
    the_string[i] = almost_return_value2[i];
  }
  *the_size = the_number;
#ifdef DEBUG_WRAP
  printf("wrap_ses_get_comments.c:  the_string is %s\n", the_string);
#endif

  return return_value;

}




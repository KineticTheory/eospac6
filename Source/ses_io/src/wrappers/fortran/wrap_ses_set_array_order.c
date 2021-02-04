

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_error_flag ses_set_array_order_(ses_file_handle* pt_handle, ses_array_order* pt_order) {
#else

#ifdef UC_UNDER
ses_error_flag SES_SET_ARRAY_ORDER_(ses_file_handle* pt_handle, ses_array_order* pt_order) {
#else

#ifdef LC_NOUNDER
ses_error_flag ses_set_array_order(ses_file_handle* pt_handle, ses_array_order* pt_order) {
#else

#ifdef UC_NOUNDER
ses_error_flag SES_SET_ARRAY_ORDER(ses_file_handle* pt_handle, ses_array_order* pt_order) {
#endif

#endif
#endif
#endif

  ses_error_flag return_value = SES_NO_ERROR;

#ifdef DEBUG_WRAP
  ses_file_handle the_handle = 0;
  the_handle = *pt_handle;
  ses_array_order the_order = 0;
  the_order = *pt_order;
  printf("wrap_ses_set_array_order.c:  the_handle is %d\n", the_handle);
  printf("wrap_ses_set_array_order.c:  the_order is %c\n", the_order);
#endif

  /*return_value = ses_set_array_order(the_handle, the_order);*/
	
#ifdef DEBUG_WRAP 
  printf("wrap_ses_set_array_order.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 

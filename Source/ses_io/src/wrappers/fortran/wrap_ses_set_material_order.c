

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_error_flag ses_set_material_order_(ses_file_handle* pt_handle) {
#else

#ifdef UC_UNDER
ses_error_flag SES_SET_MATERIAL_ORDER_(ses_file_handle* pt_handle) {
#else

#ifdef LC_NOUNDER
ses_error_flag ses_set_material_order(ses_file_handle* pt_handle) {
#else

#ifdef UC_NOUNDER
ses_error_flag SES_SET_MATERIAL_ORDER(ses_file_handle* pt_handle) {
#endif

#endif
#endif
#endif

  ses_error_flag return_value = SES_NO_ERROR;

  ses_file_handle the_handle = *pt_handle;
#ifdef DEBUG_WRAP
  printf("wrap_ses_set_material_order.c:  the_handle is %d\n", the_handle);
#endif

  return_value = ses_set_material_order(the_handle);
#ifdef DEBUG_WRAP 
  printf("wrap_ses_set_material_order.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 

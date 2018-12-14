
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_number ses_array_size_next_(ses_file_handle* pt_handle) {
#else

#ifdef UC_UNDER
ses_number SES_ARRAY_SIZE_NEXT_(ses_file_handle* pt_handle) {
#else

#ifdef LC_NOUNDER
ses_number ses_array_size_next(ses_file_handle* pt_handle) {
#else

#ifdef UC_NOUNDER
ses_number SES_ARRAY_SIZE_NEXT(ses_file_handle* pt_handle) {
#endif

#endif
#endif
#endif

  ses_number return_value = 0;

  ses_file_handle the_handle = *pt_handle;
#ifdef DEBUG_WRAP
  printf("wrap_array_size_next.c:  the_handle is %d\n", the_handle);
#endif

  return_value = ses_array_size_next(the_handle);
#ifdef DEBUG_WRAP 
  printf("wrap_array_size_next.c:  return_value is %d\n", return_value);
#endif
  return return_value;

}

 



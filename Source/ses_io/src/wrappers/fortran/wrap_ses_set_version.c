

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
  ses_error_flag ses_set_version_(ses_file_handle* pt_handle, long* the_version) {
#else

#ifdef UC_UNDER
  ses_error_flag SES_SET_VERSION_(ses_file_handle* pt_handle, long* the_version) {
#else

#ifdef LC_NOUNDER
  ses_error_flag ses_set_version(ses_file_handle* pt_handle, long* the_version) {
#else

#ifdef UC_NOUNDER
  ses_error_flag SES_SET_VERSION(ses_file_handle* pt_handle, long* the_version) {
#endif

#endif
#endif
#endif

  ses_file_handle the_handle = *pt_handle;
  long myversion = *the_version;
#ifdef DEBUG_WRAP
  printf("wrap_ses_set_version.c:  the_handle is %d\n", the_handle);
  printf("wrap_ses_set_version.c:  the_version is %d\n", the_version);
#endif

  ses_error_flag return_value = ses_set_version(the_handle, myversion);
  
 
#ifdef DEBUG_WRAP
  printf("wrap_ses_version.c:  returning error flag = %d\n", return_value);
#endif
 
  return return_value;
}

 

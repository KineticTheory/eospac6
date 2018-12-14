
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
long ses_get_version_(ses_file_handle* pt_handle) {
#else

#ifdef UC_UNDER
long SES_GET_VERSION_(ses_file_handle* pt_handle) {
#else

#ifdef LC_NOUNDER
long ses_get_version(ses_file_handle* pt_handle) {
#else

#ifdef UC_NOUNDER
long SES_GET_VERSION(ses_file_handle* pt_handle) {
#endif

#endif
#endif
#endif


  ses_file_handle the_handle = *pt_handle;

  long return_value = 0;
#ifdef DEBUG_WRAP
  printf("ses_get_version.c : the_handle is %d\n", the_handle);
#endif

  return_value = ses_get_version(the_handle);
#ifdef DEBUG_WRAP
  printf("ses_get_version.c : return_value is %d\n", return_value);
#endif

  return return_value;
}

 

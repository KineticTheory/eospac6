

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
int ses_date_(ses_file_handle* pt_handle, ses_string the_string, int* size) {
#else

#ifdef UC_UNDER
  int SES_DATE_(ses_file_handle* pt_handle, ses_string the_string, int* size) {
#else

#ifdef LC_NOUNDER
    int ses_date(ses_file_handle* pt_handle, ses_string the_string, int* size) {
#else

#ifdef UC_NOUNDER
      int SES_DATE(ses_file_handle* pt_handle, ses_string the_string, int* size) {
#endif

#endif
#endif
#endif

  int size_of_string(ses_string str);

  ses_file_handle the_handle = *pt_handle;
#ifdef DEBUG_WRAP
  printf("wrap_ses_date.c:  the_handle is %d\n", the_handle);
#endif

  ses_string almost_the_string = ses_date(the_handle);
  if (almost_the_string != (ses_string)NULL) {
    strcpy(the_string, almost_the_string);
  }
  else {
    the_string = (ses_string)NULL;
  }

  *size = strlen(the_string);

#ifdef DEBUG_WRAP
  printf("wrap_ses_date.c:  returning string = %s\n", the_string);
  printf("wrap_ses_date.c:  returning size = %d\n", *size);
#endif
 
  return size_of_string(the_string);
}

 

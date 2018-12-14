
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
int ses_get_available_formats_(ses_string the_formats) {
#else

#ifdef UC_UNDER
int SES_GET_AVAILABLE_FORMATS_(ses_string the_formats) {
#else

#ifdef LC_NOUNDER
int ses_get_available_formats(ses_string the_formats) {
#else

#ifdef UC_NOUNDER
int SES_GET_AVAILABLE_FORMATS(ses_string the_formats) {
#endif

#endif
#endif
#endif

  int size_of_string(ses_string str);

  printf("wrap_set_get_available_formats.c:  calling ses_get_available_formats\n");

  ses_string return_formats = ses_get_available_formats();

  printf("wrap_ses_get_available_formats.c:  return is %s\n", return_formats);
  strcpy(the_formats, return_formats);
  printf("wrap_ses_get_available_formats.c:  the_formats is %s\n", the_formats);

  return size_of_string(the_formats);

}


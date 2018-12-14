

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
int ses_get_label_(ses_file_handle* pt_handle, ses_label the_label) {
#else

#ifdef UC_UNDER
int SES_GET_LABEL_(ses_file_handle* pt_handle, ses_label the_label) {
#else

#ifdef LC_NOUNDER
int ses_get_label(ses_file_handle* pt_handle, ses_label the_label) {
#else

#ifdef UC_NOUNDER
int SES_GET_LABEL(ses_file_handle* pt_handle, ses_label the_label) {
#endif

#endif
#endif
#endif

  int size_of_string(ses_string str);

  ses_file_handle the_handle = *pt_handle;
#ifdef DEBUG_WRAP
  printf("wrap_ses_get_label.c:  the_handle is %d\n", the_handle);
#endif
  ses_label almost_the_label = ses_get_label(the_handle);
  if (almost_the_label != (ses_label)NULL) {
    strcpy(the_label, almost_the_label);
  }
  else {
    the_label = (ses_label)NULL;
  }

#ifdef DEBUG_WRAP 
  printf("wrap_ses_get_label.c:  the_label is %s\n", the_label);
#endif
  return size_of_string(the_label);

}

int size_of_string(char* the_string) {

   /*  return the size of a C string */

//#define DEBUG_SIZE_OF_STRING

   int return_value = 0;
   if (the_string == (ses_string)NULL) {
     return_value = 0;
#ifdef DEBUG_SIZE_OF_STRING
     printf("size_of_string -- string passed in null, returning null\n");
#endif
     return return_value;
   }

   int limit = strlen(the_string);
   int i = 0;
   while (i < limit) {
     if (the_string[i] == '\0') break;
     i++;
   }

   return_value = i;
#ifdef DEBUG_SIZE_OF_STRING
   printf("size_of_string -- return_value is %d\n", return_value);
#endif
   //if (i == limit) return_value = 0;
   return return_value;
 }

 

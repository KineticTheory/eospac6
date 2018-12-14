
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"


char*  _make_string(const char* the_string) {

  /*  given a string literal, return a string with the literal in it */

  char* return_value = (char*)NULL;


  if (the_string == 0) {
#ifdef DEBUG_PRINT
    printf("_make_string: invalid literal passed to make_string\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  int size = strlen(the_string);
  if (size > 0) {
    return_value = malloc(size*8);
    if (return_value == (char*)NULL) {
#ifdef DEBUG_PRINT
      printf("_make_string: memory allocation error in _make_string\n");
#endif
      _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
      return (char*)NULL;
    }
    strcpy(return_value, the_string);
  }
  else {
#ifdef DEBUG_PRINT
    printf("_make_string: In make_string with 0 sized literal\n");
#endif
    return_value = (char*)NULL;
  }

  return return_value;
}
 

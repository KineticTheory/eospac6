
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


#include <stdlib.h>
#include <string.h>


ses_string ses_date(ses_file_handle the_handle) {

  /*  function prototypes */

  ses_string _make_date_into_string(long the_long);
 
  /*  end function prototypes */
  
  ses_string return_value = (ses_string)NULL;


  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_date: ses file handle not valid in ses_date\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return NULL_STRING;
  }

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;

  FILE* pFILE = 0;
  pFILE = _getPFILE(pSFH);

  struct _ses_directory* ptDIR = FILE_LIST[the_handle]->_directory;

  ses_boolean read_dir = SES_FALSE;
  if (ptDIR == (struct _ses_directory*)NULL) {

    ptDIR  = _read_directory(pSFH);
    read_dir = SES_TRUE;
    if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
      printf("ses_date: did not read directory \n");
#endif
      _set_latest_error(SES_READ_ERROR);
      return (ses_string)NULL;
    }
  }

  /*  everythings ready, make the string */

  return_value = malloc(sizeof(char) * SES_MAX_STRING_SIZE);
  ses_string date_string = _make_date_into_string(ptDIR->_date);
  strcpy(return_value, date_string);
  free(date_string);
  date_string = (ses_string)NULL;
  if (read_dir == SES_TRUE) {
    _destruct_ses_directory(ptDIR);
    free(ptDIR);
    FILE_LIST[the_handle]->_directory = (struct _ses_directory*)NULL;
    ptDIR = (struct _ses_directory*)NULL;
  }


  _releasePFILE(pSFH);


  return return_value;
}


ses_string _make_date_into_string(long the_long) {

  /*  make  a long into a string */
  void itoa(long n, char s[]);

  ses_string return_value = malloc(sizeof(char)*8);
  itoa(the_long, return_value);


  return return_value;

}

void itoa(long n, char s[])
 {

     void reverse(char s[]);
     long i, sign;
 
     if ((sign = n) < 0)  /* record sign */
     {
         n = -n;          /* make n positive */
     }
     i = 0;
     do {       /* generate digits in reverse order */
         s[i++] = n % 10 + '0';   /* get next digit */
     } while ((n /= 10) > 0);     /* delete it */

     if (sign < 0)
         s[i++] = '-';
     s[i] = '\0';
     reverse(s);
 }
void reverse(char s[])
 {
     int i, j;
     char c;

     long size = (long)strlen(s);
 
     for (i = 0, j = size-1; i<j; i++, j--) {
         c = s[i];
         s[i] = s[j];
         s[j] = c;
     }
 }



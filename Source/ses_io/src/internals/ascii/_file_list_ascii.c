

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <stdio.h>
#include <math.h>
#include <string.h>

#include "_file_list_ascii.h"

//#define DEBUG_WRITE_SES_STRING_ASCII

#define check_errors_READ_SES_WORD_ARRAY_ASCII HEADER(check_errors_READ_SES_WORD_ARRAY_ASCII)
#define my_parse_doubles_from_line HEADER(my_parse_doubles_from_line)

//////  defines /////////////////////
//  MAX_BUFFER_SIZE -- for using a fixed length string buffer
/////////////////////////////////////

#define MAX_BUFFER_SIZE 300

//ses_boolean        _go_to_next_array_location_ascii(struct _ses_file_handle* pSFH, long location) {
//
//
//
//  return SES_TRUE;//
//}

double _read_double_tomax_ascii(struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_validation, int max_length) {

  /*  read a double from the current file handle */
  /*  the current code reads ascii files */

  /*  function prototypes */

  double my_truncate_for_significant_digits(double the_double, unsigned intnsig);
  ses_boolean my_do_validation_double(double the_double);

  /*  end function prototypes */

  double return_value = 0.0;

  /********************************************************************/
  /*  error check the arguments */

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_double_ascii: Null ses file handle passed to _read_double_ascii\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return return_value;
  }

  int lnsig = nsig;
  if (lnsig < 0) {
#ifdef DEBUG_PRINT
    printf("_read_double_ascii: nsig < 0, error -- changing nsig to 0\n");
#endif
    nsig = 0;
    
  }

  /*  internal error checking */

  FILE* pFILE = pSFH->_c_file_handle;
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_double_ascii: Null FILE* in _read_double\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return return_value;
  }
  /********************************************************************/

  /*  read the double as chars from the file */
  
  char myBuffer[MAX_BUFFER_SIZE];  
  char cbuffer = ' ';


  int number_read = fread(&cbuffer, 1, 1, pFILE);
  int index = 0;
#ifdef DEBUG_FILE_LIST_ASCII
      printf("first cbuffer is %c and index is %d\n", cbuffer, index);
#endif
  int tolength = 1;
  //  peel off leading blanks
  while ((cbuffer == ' ') && (number_read > 0) && (tolength < max_length)) {
      myBuffer[index] = cbuffer;
#ifdef DEBUG_FILE_LIST_ASCII
      printf("peeling cbuffer is %c and index is %d\n", cbuffer, index);
#endif
      index++;
       number_read = fread(&cbuffer, 1, 1, pFILE);
      tolength++;
  }
  //  read the double as a string
  //while ((cbuffer != ' ') && (number_read > 0) && (tolength < max_length)) {
  while ((number_read > 0) && (tolength < max_length)) {

      myBuffer[index] = cbuffer;
#ifdef DEBUG_FILE_LIST_ASCII
      printf("string cbuffer is %c and index is %d tolength is %d\n", cbuffer, index, tolength);
#endif
      index++;
      number_read = fread(&cbuffer, 1, 1, pFILE);
      tolength++;
    
  }
  if (tolength == max_length) {
#ifdef DEBUG_FILE_LIST_ASCII
      printf("equals max_length cbuffer is %c and index is %d tolength is %d\n", cbuffer, index, tolength);
#endif
	myBuffer[index] = cbuffer;
  }
  index++;
  myBuffer[index] = '\0';

  /* convert the input character array to a double  */

  double mydBuffer = 0.0;

  char* the_string = &myBuffer[0];
#ifdef DEBUG_FILE_LIST_ASCII
  printf("the_string is |1234567890123456789012|\n");
  printf("the_string is |%s|\n", the_string);
#endif
  mydBuffer = strtod(the_string, NULL);

  double tmp_double;

  /*  truncate for significant digits */
  if (nsig != 0) {
      tmp_double = my_truncate_for_significant_digits(mydBuffer, nsig);
      return_value = tmp_double;
  }
  else {
      return_value = mydBuffer;
  }

  /*  do validation */

  if (do_validation == SES_TRUE) {
      if (my_do_validation_double(return_value) == SES_FALSE) {
#ifdef DEBUG_PRINT
        printf("_read_double_ascii: do validation error on double -- setting error\n");
#endif
        _set_latest_error(SES_READ_ERROR);
        return_value = 0.0;
      }
  }
  
  return return_value;

}

double _read_double_ascii(struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_validation) {

  /*  read a double from the current file handle */
  /*  the current code reads ascii files */

  /*  function prototypes */

  double my_truncate_for_significant_digits(double the_double, unsigned intnsig);
  ses_boolean my_do_validation_double(double the_double);

  /*  end function prototypes */

  double return_value = 0.0;

  /********************************************************************/
  /*  error check the arguments */

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_double_ascii: Null ses file handle passed to _read_double_ascii\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return return_value;
  }

  int lnsig = nsig;
  if (lnsig < 0) {
#ifdef DEBUG_PRINT
    printf("_read_double_ascii: nsig < 0, error -- changing nsig to 0\n");
#endif
    nsig = 0;
    
  }

  /*  internal error checking */

  FILE* pFILE = pSFH->_c_file_handle;
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_double_ascii: Null FILE* in _read_double\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return return_value;
  }
  /********************************************************************/

  /*  read the double as chars from the file */
  
  char myBuffer[MAX_BUFFER_SIZE];  
  char cbuffer = ' ';


  int number_read = fread(&cbuffer, 1, 1, pFILE);
  int index = 0;
  //  peel off leading blanks
  while ((cbuffer == ' ') && (number_read > 0)) {
      myBuffer[index] = cbuffer;
      index++;
      number_read = fread(&cbuffer, 1, 1, pFILE);
      
  }
  //  read the double as a string
  while ((cbuffer != ' ') && (number_read > 0) && (index < MAX_BUFFER_SIZE)) {
      myBuffer[index] = cbuffer;
      index++;
      number_read = fread(&cbuffer, 1, 1, pFILE);
  }
   
  myBuffer[index] = '\0';

  /* convert the input character array to a double  */

  double mydBuffer = 0.0;

  char* the_string = &myBuffer[0];
  mydBuffer = strtod(the_string, NULL);

  double tmp_double;

  /*  truncate for significant digits */
  if (nsig != 0) {
      tmp_double = my_truncate_for_significant_digits(mydBuffer, nsig);
      return_value = tmp_double;
  }
  else {
      return_value = mydBuffer;
  }

  /*  do validation */

  if (do_validation == SES_TRUE) {
      if (my_do_validation_double(return_value) == SES_FALSE) {
#ifdef DEBUG_PRINT
        printf("_read_double_ascii: do validation error on double -- setting error\n");
#endif
        _set_latest_error(SES_READ_ERROR);
        return_value = 0.0;
      }
  }
  
  return return_value;

}


ses_boolean _write_double_tomax_ascii(struct _ses_file_handle* pSFH, double the_double, unsigned int nsig, ses_boolean do_validation, int size) {
  /*  write a double to the current file handle */

  /*  function prototypes */

  double my_truncate_for_significant_digits(double the_double, unsigned intnsig);

  /*  end function prototypes */

  ses_boolean return_value = SES_FALSE;

  /********************************************************************/
  /*error check the arguments */


  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_double_ascii:  null ses file handle into _write_double_ascii\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  int lnsig;
  lnsig = (int)nsig;
  if (lnsig < 0) {
#ifdef DEBUG_PRINT
    printf("_write_double_ascii: nsig < 0, changing to 0 (no truncation)\n");
#endif
    nsig = 0;
  }
  FILE* pFILE = pSFH->_c_file_handle;
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_double_ascii: Null FILE* in _write_double\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return return_value;
  }
  /********************************************************************/

  if (do_validation == SES_TRUE) {
      if (my_do_validation_double(the_double) == SES_FALSE) {
#ifdef DEBUG_PRINT
        printf("_write_double_ascii: do validation error on double -- setting error\n");
#endif
        _set_latest_error(SES_WRITE_ERROR);
        return_value = SES_FALSE;
      }    
  }

  double the_trunc =  my_truncate_for_significant_digits(the_double, nsig);
  double local_double  = the_trunc;



  /*  convert the double to a string */


  char the_string[MAX_BUFFER_SIZE];
  char format[8];
  int the_size1 = 0;
  the_size1 = sprintf(format," %2d.15E", size);
  format[0] = '%';
  format[7] = '\0';

  int the_size = snprintf(the_string, MAX_BUFFER_SIZE, &format[0], local_double);
  the_string[the_size] = '\0';
  /*  write the string */

  int number_written = 0;

  if (the_size > 0) {

	char* new_string = malloc(sizeof(char) * MAX_BUFFER_SIZE);
	strcpy(new_string, the_string);
	char* number = &new_string[0];
	char* almost_exponent = strstr(new_string, "E");
	char* exponent = malloc(sizeof(char) * 25);
	if (almost_exponent != (char*)NULL) {
		strcpy(exponent, almost_exponent);
	}
	else {
		strcpy(exponent, "");
	}
	int index_E = (almost_exponent - number)/(sizeof(char));
	if (index_E >= the_size) {
		number_written = 0;
	}
        else {
		number[index_E] = '\0';
	
		int len_number = strlen(number);
		int len_exponent = strlen(exponent);
		if ((len_number != 18) || (len_exponent != 4)) {
			if (len_exponent == 5) {
				strcpy(&the_string[0], " ");
				strcat(&the_string[0], number);
				strcat(&the_string[0], exponent + sizeof(char));
			}
			number_written = fwrite(&the_string[0], 1, the_size, pFILE);
			
		}
		else {
			number_written = fwrite(&the_string[0], 1, the_size, pFILE);
		}

		free(number);
		free(exponent);
	}
  
  }
  else {
	number_written = 0;
  }

  return_value = SES_TRUE;
 
  return return_value;


}

ses_boolean _write_double_ascii(struct _ses_file_handle* pSFH, double the_double, unsigned int nsig, ses_boolean do_validation) {

  /*  write a double to the current file handle */

  /*  function prototypes */

  double my_truncate_for_significant_digits(double the_double, unsigned intnsig);

  /*  end function prototypes */

  ses_boolean return_value = SES_FALSE;

  /********************************************************************/
  /*error check the arguments */


  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_double_ascii:  null ses file handle into _write_double_ascii\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  int lnsig;
  lnsig = (int)nsig;
  if (lnsig < 0) {
#ifdef DEBUG_PRINT
    printf("_write_double_ascii: nsig < 0, changing to 0 (no truncation)\n");
#endif
    nsig = 0;
  }
  FILE* pFILE = pSFH->_c_file_handle;
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_double_ascii: Null FILE* in _write_double\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return return_value;
  }
  /********************************************************************/

  if (do_validation == SES_TRUE) {
      if (my_do_validation_double(the_double) == SES_FALSE) {
#ifdef DEBUG_PRINT
        printf("_write_double_ascii: do validation error on double -- setting error\n");
#endif
        _set_latest_error(SES_WRITE_ERROR);
        return_value = SES_FALSE;
      }    
  }

  double the_trunc =  my_truncate_for_significant_digits(the_double, nsig);
  double local_double  = the_trunc;


  /*  convert the double to a string */


  char the_string[MAX_BUFFER_SIZE];
  int the_size = sprintf(the_string,"%e", local_double);

  /*  write the string */

  int number_written = 0;
  if (the_size > 0) {
  	number_written = fwrite(&the_string, 1, the_size, pFILE);
  }
  else {
	number_written = 0;
  }
  /*  write a space */
  char MYSPACE = ' ';
  number_written = fwrite(&MYSPACE,1,1,pFILE);
  if (number_written <= 0) {
#ifdef DEBUG_PRINT
    printf("_write_double_ascii: Failure in writing double to pFILE\n");
#endif
    _set_latest_error(SES_WRITE_ERROR);
    return_value = SES_FALSE;
  }
  else {
    return_value = SES_TRUE;
  }
 
  return return_value;


}

long _read_long_ascii(struct _ses_file_handle* pSFH) {

  /*  read a long as an ascii from the current file handle */

  long return_value;

  /********************************************************************/
  /*  error check the arguments */

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_long_ascii: ses file handle null in _read_long\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  FILE* pFILE = pSFH->_c_file_handle;

  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_long_ascii: null c file handle into _read_long_ascii\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }
  /********************************************************************/

  /*  read the long as a character string from the file */

  long mylBuffer;
  char myBuffer[MAX_BUFFER_SIZE];  
  char cbuffer = ' ';
  ses_boolean at_start = SES_TRUE;

  int number_read = fread(&cbuffer, 1, 1, pFILE);
  int index = 0;
  while ( (((int)cbuffer != 32) || (at_start == SES_TRUE)) && (number_read > 0)) {

    myBuffer[index] = cbuffer;
    index++;
    number_read = fread(&cbuffer, 1, 1, pFILE);
    if ((int)cbuffer != 32) {
	at_start = SES_FALSE;
    }
    
  }
   
  myBuffer[index] = '\0';
  char* the_string = &myBuffer[0];

  /* convert the input character stream to a long  */
  mylBuffer = 0.0;

  double mydBuffer = 0.0;
  mydBuffer = strtod(the_string, NULL);
  mylBuffer = (long)mydBuffer;

  if (index <= 0) {
#ifdef DEBUG_PRINT
    printf("_read_long_ascii: Failure in reading long\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return_value = 0;
  }
  else {
    return_value = mylBuffer;
  }

  return return_value;


}

ses_boolean _write_long_ascii(struct _ses_file_handle* pSFH, long the_long) {

  /*  write a long to the current file handle */

  ses_boolean return_value = SES_FALSE;

  /********************************************************************/

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_long_ascii: null ses file handle into _write_long\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  FILE* pFILE = pSFH->_c_file_handle;
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_long_ascii: Null FILE* in _write_long\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return return_value;
  }
  /********************************************************************/

  long local_long = the_long;

  /*  convert the long to a string */

  char the_string[MAX_BUFFER_SIZE];
  int the_size = sprintf(the_string,"%ld", local_long);
 
  /*  write the string */

  int number_written = fwrite(&the_string, 1, the_size, pFILE);

  /*  write a space */
  char MYSPACE = ' ';
  number_written = fwrite(&MYSPACE,1,1,pFILE);
  if (number_written <= 0) {
#ifdef DEBUG_PRINT
    printf("_write_long_ascii: Failure in writing long to pFILE\n");
#endif
    _set_latest_error(SES_WRITE_ERROR);
    return_value = SES_FALSE;
  }
  else {
    return_value = SES_TRUE;
  }

  return return_value;


}


long _read_long_pFILE_ascii(FILE* pFILE, ses_boolean needs_flip) {

  /*  read a long from the current file handle */

  long return_value;

  /*  error check the arguments */


 /********************************************************************/
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_long_pFILE_ascii: null c file handle into _read_long_pFILE_ascii\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }
 /********************************************************************/

  /*  read the long as a character string from the file */

  long mylBuffer;
  char myBuffer[MAX_BUFFER_SIZE];  
  char cbuffer = ' ';
  int index = 0;

  int number_read = fread(&cbuffer, 1, 1, pFILE);
  if ((cbuffer == ' ') && (number_read > 0)) {
    number_read = fread(&cbuffer, 1,1, pFILE);
  }
  while ((cbuffer != ' ') && (number_read > 0)) {
    myBuffer[index] = cbuffer;
    index++;
    number_read = fread(&cbuffer, 1, 1, pFILE);
    
  }
   
  myBuffer[index] = '\0';
  char* the_string = &myBuffer[0];

  /* convert the input character stream to a long  */
  mylBuffer = 0;

  double myldBuffer = atof(the_string);
  mylBuffer = (long)myldBuffer;

  if (index < 0) {
#ifdef DEBUG_PRINT
    printf("_read_long_pFILE_ascii: Failure in reading long\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return_value = 0;
  }
  else {
    return_value = mylBuffer;
  }

  return return_value;


}

ses_boolean  _write_long_pFILE_ascii(FILE* pFILE, ses_boolean needs_flip) {
  return SES_FALSE;
}

char _read_char_ascii(FILE* pFILE ) {

  /*  read a char from the current c file handle */

  char return_value = (char)0;

 /********************************************************************/
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_char_ascii: null c file handle into _read_char\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }
 /********************************************************************/

  char tmp = (char)0;
  int number_read = fread(&tmp, 1, 1, pFILE);
  if (number_read <= 0) {
#ifdef DEBUG_PRINT
    printf("_read_char_ascii: Failure in reading char from pFILE\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return_value = 0;
  }
  else {
    return_value = tmp;
  }
  return return_value;
}

ses_boolean _write_char_ascii(FILE* pFILE, char the_char) {

  /*  write a char to the current file handle */
  /*  note:  char's do NOT need flipping */


  ses_boolean return_value = SES_FALSE;
 /********************************************************************/
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_char_ascii: null c file handle into _write_char\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }
 /********************************************************************/

  int number_written = fwrite(&the_char, 1, 1, pFILE);
  if (number_written <= 0) {
#ifdef DEBUG_PRINT
    printf("_write_char_ascii: Failure in writing char to pFILE\n");
#endif
    _set_latest_error(SES_WRITE_ERROR);
    return_value = SES_FALSE;
  }
  else {
    return_value = SES_TRUE;
  }

  return return_value;


}

int  my_parse_doubles_from_line(char* line, ses_word_reference the_array, int double_index, int size_double, int size_array, FILE* cfile) {

  //  from the line, split into doubles by length (fixed format parse)

  int i;
  int start_index = 0;
  char* the_double_as_string = (char*)NULL;
  int array_index = double_index;
  int start_with = 0;
  if (line[0] == ' ') {
    start_with = 1;
    start_index = start_with;
  }
  double mydBuffer = 0.0;


  int number_elements = strlen(line)/size_double;
  
  start_index = 0;

  char* pEnd;
  for (i = 0; i < number_elements; i++) {

//printf("line %s i %d array_index %d size_array%d size_double %d\n", line, i, array_index, size_array, size_double);
       if (array_index < size_array) {

//if (array_index >= size_array - 2) {
//printf("size_double is %d line is %s\n", size_double, line);
//}
          if (size_double > 0) {
          	the_double_as_string = malloc(sizeof(char)*(size_double+1));
		if (the_double_as_string != (char*)NULL) {
          		strncpy(the_double_as_string, &line[start_index], size_double);
		        the_double_as_string[size_double] = '\0';
//if (array_index >= size_array - 2) {
//printf("the_double_as_string is %s\n", the_double_as_string);
//}
                	mydBuffer = strtod(the_double_as_string, &pEnd);

                	the_array[array_index] = mydBuffer;
//if (array_index >= size_array - 2) {
//printf("the_array[%d] size_array %d is %e\n", array_index, size_array, mydBuffer);
//printf("line is %s\n", line);
//}

                	free(the_double_as_string);
                	the_double_as_string = (char*)NULL;
		}
	  }

       }
       array_index++;

       start_index = start_index + size_double;
  }


  
  return array_index;
}

ses_word_reference _read_ses_word_array_ascii(struct _ses_file_handle* pSFH, long size_array, unsigned int nsig, ses_boolean do_validation) {

  /*  function prototypes */

  ses_word_reference check_errors_READ_SES_WORD_ARRAY_ASCII(struct _ses_file_handle* pSFH, long size );
  int  my_parse_doubles_from_line(char* line, ses_word_reference the_array, int double_index, int size_double, int size_array, FILE* cfile);

  /*  end function prototypes */

  ses_word_reference input_errors = check_errors_READ_SES_WORD_ARRAY_ASCII(pSFH, size_array);
  if (input_errors != SES_NO_ERROR) {
	return (ses_word_reference)NULL;
  }

  /*  read a ses word array from the current file handle */

  ses_word_reference return_value = (ses_word_reference)NULL;

  /*  allocate the memory for the return */

  return_value = malloc(sizeof(ses_word)*size_array);
  int i2 = 0;
  for (i2=0; i2< size_array; i2++) {
	return_value[i2] = 0.0;
  }
  if (return_value == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_ses_word_array_ascii: memory allocation error\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (ses_word_reference)NULL;
  }


  int num_lines = 0;
  int current_location = ftell(pSFH->_c_file_handle);

//  char the_char = ' ';

  //  peel off the leading blanks
  //while (the_char == ' ') {
//	current_location = ftell(pSFH->_c_file_handle);
//	the_char = _read_char_ascii(pSFH->_c_file_handle);
//  }  

  //  move back to current location 
  //fseek(pSFH->_c_file_handle, current_location, SEEK_SET);

  double tmp = _read_double_tomax_ascii(pSFH, nsig, do_validation,pSFH->_word_size);

  if (size_array == 1) {
	return_value[0] = tmp;
	return return_value;
  }


  int next_location = ftell(pSFH->_c_file_handle);
  int size_double_ascii = next_location - current_location;

  /*  if at end of line where we have 11111\n -- resize */
  
  if (tmp > 3.0e+300) {
	size_double_ascii = size_double_ascii - 6;
  }

  pSFH->_word_size = size_double_ascii;
  fseek(pSFH->_c_file_handle, current_location, SEEK_SET);

  int i = 0;
  char line[128];
  /*  initialize */
  for (i=0; i < 127; i++) {
	line[i] = ' ';
  }
  line[127] = '\0';
  
  int num_per_line = (128/size_double_ascii);

  num_lines = size_array/num_per_line;
//printf("num_lines is %d size_arrays is %d num_per_line is %d \n", num_lines, size_array, num_per_line);
  if (num_lines*num_per_line  < 0) {
	num_lines++;
  }
  

  int j = 0;
  int start_index = 0;
  int double_index = 0;
  for (i=0; i < num_lines; i++) {
     current_location = ftell(pSFH->_c_file_handle);
     fgets ( &line[0], sizeof line, pSFH->_c_file_handle);
     for (j = 127; j >= 0; j--) {
	if (line[j] == '1') break;

     }
     start_index = j - 4;
     //  strip the crap off the end of the line
     line[122] = ' ';
     line[123] = ' ';
     line[124] = ' ';
     line[125] = ' ';
     line[126] = ' ';
     line[127] = '\0';

     //  parse the doubles off the line
//printf("parsing %s\n", line);
     int next_index = my_parse_doubles_from_line(&line[0], return_value, double_index, size_double_ascii, size_array, pSFH->_c_file_handle);


     double_index = next_index;
//printf("next_index is %d\n", next_index);
     if ((i == num_lines-1) && (next_index < size_array)) {
	num_lines++;
     }
     if (next_index > size_array) {
 	//  on the last line, you've read some of the 'next' array
	//  need to move the file handle back

	fseek(pSFH->_c_file_handle, current_location , SEEK_SET);
        int i = 0;
	int many = num_per_line - (next_index - size_array)  ;
        for (i = 0; i < many; i++) {
  		tmp = _read_double_tomax_ascii(pSFH, nsig, do_validation, pSFH->_word_size);

	}

	
 	
     }


     

 }

 
//#define DEBUG0
#ifdef DEBUG0

	printf("return_value[%d] is %e return_value[%d] is %e\n", size_array - 2, return_value[size_array-2], size_array-1, return_value[size_array-1]);
	if (return_value[size_array-1] == 0.0) {
//exit(0);
	}
#endif


  return return_value;
}

ses_word_reference check_errors_READ_SES_WORD_ARRAY_ASCII(struct _ses_file_handle* pSFH, long size_array ) {

  /*  error checks upon entrance to _read_ses_word_array_ascii */
 
  /********************************************************************/
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_ses_word_array_ascii: ses file handle null in _read_ses_word_array\n");
#endif
    return (ses_word_reference)NULL;
  }


  if (size_array <= 0) {
#ifdef DEBUG_PRINT
    printf("_read_ses_word_array_ascii: size array <= 0\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (ses_word_reference)NULL;
  }
  /********************************************************************/


   return SES_NO_ERROR;
}


ses_boolean _write_ses_word_array_ascii(struct _ses_file_handle* pSFH, ses_word the_array[], long size_array, unsigned int nsig, ses_boolean do_validation) {

  /*  write an array of ses_word's to the current file handle */

  ses_boolean return_value = SES_TRUE;

 /********************************************************************/
  /*  error check the arguments */

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_ses_word_array_ascii: null ses file handle\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (size_array <= 0) {
#ifdef DEBUG_PRINT
    printf("_write_ses_word_array_ascii: size array <= 0\n");
#endif
    _set_latest_error(SES_ARRAY_SIZE_ERROR);
    return SES_FALSE;
  }
 /********************************************************************/

  /*  write the array */

  long i = 0;
  ses_boolean tmp = SES_TRUE;

  int start_index = pSFH->_start_index;
  ses_boolean write_marker = SES_FALSE;

  for (i=0; i < size_array; i++) {


   if ((i+start_index)%5 == 4) {
	write_marker = SES_TRUE;
   }

   tmp = _write_double_tomax_ascii(pSFH, the_array[i], nsig, do_validation, pSFH->_word_size);
   if (write_marker == SES_TRUE) {
       ses_boolean didit = _write_char_ascii(pSFH->_c_file_handle, '1');
       didit = _write_char_ascii(pSFH->_c_file_handle, '1');
       didit = _write_char_ascii(pSFH->_c_file_handle, '1');
       didit = _write_char_ascii(pSFH->_c_file_handle, '1');
       didit = _write_char_ascii(pSFH->_c_file_handle, '1');
       didit = _write_char_ascii(pSFH->_c_file_handle, '\n');
       pSFH->_start_index = 0;
   }
   else {
	if (i == (size_array - 1)) {
		if (size_array < 5) {
			pSFH->_start_index += size_array;
		}
		else {
			pSFH->_start_index = (start_index + size_array)%5;
		}
                //ses_boolean didit = _write_char_ascii(pSFH->_c_file_handle, '\n');
	}
   }


   if (tmp == SES_FALSE) {
#ifdef DEBUG_PRINT
      printf("_write_ses_word_array_ascii: _write_double_ascii failed\n");
#endif
      _set_latest_error(SES_WRITE_ERROR);
      return SES_FALSE;
    }

    write_marker = SES_FALSE;
  }

  return return_value;
}

ses_boolean        _write_ses_string_ascii(struct _ses_file_handle* pSFH, ses_string the_string, long size_array){

  ses_boolean return_value = SES_TRUE;

 /********************************************************************/
  if (the_string == (ses_string)NULL) {
#ifdef DEBUG_PRINT
     printf("_write_ses_string_ascii:  the string is null\n");
#endif
     _set_latest_error(SES_NULL_OBJECT_ERROR);
     return SES_FALSE;
  }
 /********************************************************************/
#ifdef DEBUG_WRITE_SES_STRING_ASCII
  printf("at top _write_ses_string_ascii, the_string is |%s| size_array si %ld strlen is %ld\n", the_string, size_array, strlen(the_string));
#endif


  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_ses_string_ascii: null ses file handle\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }
  FILE* pFILE = pSFH->_c_file_handle;
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_ses_string_ascii: Null FILE* in _write_ses_string_ascii\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  long size_string = size_array;


  long i = 0;
  ses_boolean tmp = SES_TRUE;
  if (size_array > size_string) {
	int start = size_string;
	for (i=start; i < size_array; i++) {
		the_string[i] = ' ';
	}
  }
#ifdef DEBUG_WRITE_SES_STRING_ASCII
  printf("_write_ses_string_ascii, size_array is %ld the_string is |%s|\n", size_array, the_string);
#endif
  for (i=0; i < size_array; i++) {

   tmp = _write_char_ascii(pFILE, the_string[i]);
   if ((i+1)%80 == 0) {
        tmp = _write_char_ascii(pFILE, '\n');
   }

   if (tmp == SES_FALSE) {
#ifdef DEBUG_PRINT
      printf("_write_ses_string_ascii: _write_char_ascii failed\n");
#endif
      _set_latest_error(SES_WRITE_ERROR);
      return SES_FALSE;
    }
  }
  if (size_array%80 != 0) {
        tmp = _write_char_ascii(pFILE, '\n');
  }

  return return_value;
	
}

ses_error_flag        _write_eof_ascii(struct _ses_file_handle* pSFH) {

  /*  function prototypes */

  ses_boolean _write_char_ascii(FILE* pFILE, char the_char);

  /*  end function prototypes */



  ses_string the_string = malloc(sizeof(char*)*80);
  strcpy(the_string, " 2                                                                             2");
  ses_boolean didit_write2 = _write_ses_string_ascii(pSFH, the_string, 80);
  free(the_string);
  the_string = (ses_string)NULL;
  if (didit_write2 == SES_FALSE) {
#ifdef DEBUG_PRINT
     printf("_write_eof_ascii:  second eof mark at end of file not written correctly\n");
#endif
    _set_latest_error(SES_WRITE_ERROR);
    return SES_WRITE_ERROR;
  }





  return SES_NO_ERROR;
}






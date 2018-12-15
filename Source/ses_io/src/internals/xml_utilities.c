
//  xml utilities

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "xml_utilities.h"
#include <string.h>

#define check_errors_read_long_tag HEADER(check_errors_read_long_tag)
#define check_errors_read_long_list_tag HEADER(check_errors_read_long_list_tag)
#define check_errors_write_tag HEADER(check_errors_write_tag)
#define check_errors_skip_tag HEADER(check_errors_skip_tag)
#define check_errors_read_char_pFILE_xml HEADER(check_errors_read_char_pFILE_xml)
#define check_errors_read_char_xml HEADER(check_errors_read_char_xml)
#define check_errors_read_long_pFILE_xml HEADER(check_errors_read_long_pFILE_xml)
#define check_errors_write_long_pFILE_xml HEADER(check_errors_write_long_pFILE_xml)
#define check_errors_read_long_xml HEADER(check_errors_read_long_xml)
#define check_errors_write_long_xml HEADER(check_errors_write_long_xml)
#define check_errors_read_double_pFILE_xml HEADER(check_errors_read_double_pFILE_xml)
#define check_errors_write_double_pFILE_xml HEADER(check_errors_write_double_pFILE_xml)
#define check_errors_read_double_xml HEADER(check_errors_read_double_xml)
#define check_errors_write_double_pFILE_xml HEADER(check_errors_write_double_pFILE_xml)
#define check_errors_read_double_xml HEADER(check_errors_read_double_xml)
#define check_errors_write_double_xml HEADER(check_errors_write_double_xml)
#define check_errors_read_word_pFILE_xml HEADER(check_errors_read_word_pFILE_xml)
#define check_errors_read_word_list_pFILE_xml HEADER(check_errors_read_word_list_pFILE_xml)
#define check_errors_read_ses_word_array_xml HEADER(check_errors_read_ses_word_array_xml)
#define check_errors_write_ses_word_array_xml HEADER(xcheck_errors_write_ses_word_array_xml)
#define check_errors_write_ses_string_xml HEADER(check_errors_write_ses_string_xml)
/***************************************************/
long _read_long_tag(FILE* pFILE, char* tag_name){

  //  read an XML tag with the tag <tag_name> and return the first long contained within

  /*  function prototypes */

  ses_error_flag check_errors_read_long_tag(FILE* pFILE, char* tag_name);

  /*  end function prototypes */

  long return_value = 0;

  ses_error_flag check = check_errors_read_long_tag(pFILE, tag_name);
  if (check != SES_NO_ERROR) {
	return 0;
  }

  /*  position the file handle after the <tag_name> tag */

  char* the_tag = _skip_tag(pFILE, tag_name);
  if (the_tag == (char*)NULL) {
	return 0;
  }

  /*  read the long */

  if (strstr(the_tag, tag_name) != NULL) {

    /*   the_tag contains tag_name -- capture the value*/

     return_value =  _read_long_pFILE_xml(pFILE);
  }

  /*  clean up memory */

  free(the_tag);
  the_tag = (char*)NULL;

  /*  position the file handle after the </tag_name> tag */

  the_tag = _skip_tag(pFILE, tag_name);
  free(the_tag);
  the_tag = (char*)NULL;

  return return_value;
}

ses_error_flag check_errors_read_long_tag(FILE* pFILE, char* tag_name) {

  if (pFILE == (FILE*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  if (tag_name == (char*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  return SES_NO_ERROR;
}
/*******************************/

long* _read_long_list_tag(FILE* pFILE, char* tag_name, int num) {

  //  read an XML tag with the tag <tag_name> and return the num longs contained within

  /*  function prototypes */

  ses_error_flag check_errors_read_long_list_tag(FILE* pFILE, char* tag_name, int num);

  /*  end function prototypes */

  ses_error_flag check = check_errors_read_long_list_tag(pFILE, tag_name, num);
  if (check != SES_NO_ERROR) {
	return 0;
  }

  long* return_value = malloc(sizeof(long) * num);

  /*  position the file handle after the <tag_name> tag */

  char* the_tag = _skip_tag(pFILE, tag_name);
  if (the_tag == (char*)NULL) {
       free(return_value);
       return_value = (long*)NULL;
       return return_value;
  }

  /*  read the long list */

  int i = 0;
  for (i = 0;  i < num; i++) {
    return_value[i] = _read_long_pFILE_xml(pFILE);
  }

  /*  position the file handle after the </tag_name> tag */

  free(the_tag);
  the_tag = (char*)NULL;
  the_tag = _skip_tag(pFILE, tag_name);

  /*  clean up memory */

  free(the_tag);
  the_tag = (char*)NULL;

  return return_value;
}

ses_error_flag check_errors_read_long_list_tag(FILE* pFILE, char* tag_name, int num) {

  if (pFILE == (FILE*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  if (tag_name == (char*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  if (num <= 0) {
	return SES_OBJECT_OUT_OF_RANGE;
  }
  return SES_NO_ERROR;
}

/************************************************/

ses_boolean _write_tag(FILE* pFILE, char* the_tag) {

  //  routine to write a tag to a pFILE

  /*  function prototypes */

  ses_error_flag check_errors_write_tag(FILE* pFILE, char* the_tag);

  /*  end function prototypes */

  ses_error_flag check = check_errors_write_tag(pFILE, the_tag);
  if (check != SES_NO_ERROR) {
	return check;
  }

  int i = 0;

  if (the_tag != (char*)NULL) {
  	int num = strlen(the_tag);
	if (num > 0) {
  		for (i=0; i < num; i++) {
  		  _write_char(pFILE, the_tag[i]);
  		}
	}
  }

  return SES_TRUE;  
}

ses_error_flag check_errors_write_tag(FILE* pFILE, char* the_tag) {

  if (pFILE == (FILE*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  if (the_tag == (char*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  return SES_NO_ERROR;
}

/************************************************/

char* _skip_tag(FILE* pFILE, char* tag) {

  /*  position the file pointer after the next tag <tagname> */
  /*      <tag_name>   35   </tag_name>
                    ^
                  it would be positioned here
  */

  /*  function prototypes */

  ses_error_flag check_errors_skip_tag(FILE* pFILE, char* the_tag);

  /*  end function prototypes */

  ses_error_flag check = check_errors_skip_tag(pFILE, tag);
  if (check != SES_NO_ERROR) {
	return (char*)NULL;
  }

  char* return_value = (char*)NULL;
  const int BUFFER_LENGTH = 1000;

  char* buffer = malloc(sizeof(char) * BUFFER_LENGTH);
  int i = 0;
  for (i = 0; i < BUFFER_LENGTH; i++) {
	buffer[i] = '\0';
  }

  /*  read a string in from the current file handle into the buffer
      (ending delimiter is '>') */

  char c = ' ';
  int length = 0;
  c = _read_char(pFILE);

  i = 0;
  ses_boolean saw_end = SES_FALSE;
  while ((i < BUFFER_LENGTH - 2) && (!feof(pFILE))) {
    buffer[i] = c;
    if (c == '>') {
	saw_end = SES_TRUE;
	break;
    }
    else {
    	i++;
    	c = _read_char(pFILE);
    }
  }

  i = i + 1;
  buffer[i] = '\0';
  length = i + 1;

  /*  keep going until tag_name found */

  char* new_buffer = (char*)NULL;
  if (saw_end == SES_TRUE) {
	if ((buffer != (char*)NULL) && (strstr(buffer, tag) == (char*)NULL)) {
                //  buffer is NOT NULL and tag is not in buffer
		new_buffer = _skip_tag(pFILE, tag);
		if (new_buffer != (char*)NULL) {
                        free(buffer);
		        buffer = (char*)NULL;
			buffer = malloc(sizeof(char) * (strlen(new_buffer) + 1));
			for (i = 0; i < strlen(new_buffer) + 1; i++) {
				buffer[i] = new_buffer[i];
			}	
			free(new_buffer);
			new_buffer = (char*)NULL;
		}
       }
  }

  /*  create the output */

  if (saw_end == SES_TRUE) {
      return_value = buffer;
  }
  else {
        free(buffer);
	buffer = (char*)NULL;
	return_value = (char*)NULL;
  }

  /*  return the output */

  return return_value;

}

ses_error_flag check_errors_skip_tag(FILE* pFILE, char* the_tag) {

  if (pFILE == (FILE*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  if (the_tag == (char*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  if (feof(pFILE) ) {
	return SES_FILE_READY_ERROR;
  } 

  return SES_NO_ERROR;
}

/************************************************/

char               _read_char_pFILE_xml(FILE* pFILE) {

  /* from the current c file handle (pFILE), read in a char
     and return it */

  /*  function prototypes */

  ses_error_flag check_errors_read_char_pFILE_xml(FILE* pFILE);

  /*  end function prototypes */

  ses_error_flag check = check_errors_read_char_pFILE_xml(pFILE);
  if (check != SES_NO_ERROR) {
	return ' ';
  }

  char return_value = _read_char(pFILE);

  /*  return the char */

  return return_value;

}
ses_error_flag check_errors_read_char_pFILE_xml(FILE* pFILE) {
  if (pFILE == (FILE*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  return SES_NO_ERROR;
}

/*************************************************/

//ses_boolean        _write_char_pFILE_xml(FILE* pFILE, char the_char);

/**************************************************/

char               _read_char_xml(struct _ses_file_handle* pSFH){

 /*  function prototypes */

  ses_error_flag check_errors_read_char_xml(struct _ses_file_handle* pSFH);

  /*  end function prototypes */

  ses_error_flag check = check_errors_read_char_xml(pSFH);
  if (check != SES_NO_ERROR) {
	return ' ';
  }

  return _read_char_pFILE_xml(pSFH->_c_file_handle);
	
}
ses_error_flag check_errors_read_char_xml(struct _ses_file_handle* pSFH) {
  if (pSFH == (struct _ses_file_handle*)NULL) {
        return SES_NULL_OBJECT_ERROR;
  }
  if (pSFH->_c_file_handle == (FILE*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  return SES_NO_ERROR;
}

/**********************************************************************/
//ses_boolean        _write_char_xml(struct _ses_file_handle* pSFH, char the_char);

/************************************************/


long _read_long_pFILE_xml(FILE* pFILE) {

  /* from the current c file handle (pFILE), read a long in text, convert to long,
     and return it */

  /*  function prototypes */

  ses_error_flag check_errors_read_long_pFILE_xml(FILE* pFILE);

  /*  end function prototypes */

  ses_error_flag check = check_errors_read_long_pFILE_xml(pFILE);
  if (check != SES_NO_ERROR) {
	return check;
  }

  long return_value = 0;
  const int BUFFER_LENGTH = 100;
#ifdef _MSC_VER
  char buffer[100];
#else
  char buffer[BUFFER_LENGTH];
#endif

  /*  read a string in from the current file handle into the buffer (ending delimiter is blank space)*/

  char c = _read_char(pFILE);
  while ((c == ' ') || (c == '\n')) {
    c = _read_char(pFILE);
  }

  /*  after blank space  */

  int i = 0;
 
  while ((i < BUFFER_LENGTH - 10) && (c  != ' ')) {
    buffer[i] = c;
    i++;
    c = _read_char(pFILE);
  }
  buffer[i] = '\0';

  /*  convert the buffer to a long */

  return_value = atol(&buffer[0]);

  /*  return the long */

  return return_value;
}
ses_error_flag check_errors_read_long_pFILE_xml(FILE* pFILE) {

  if (pFILE == (FILE*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  return SES_NO_ERROR;
}

/************************************************/

ses_boolean _write_long_pFILE_xml(FILE* pFILE, long the_long) {

  /*  function prototypes */

  ses_error_flag check_errors_write_long_pFILE_xml(FILE* pFILE);

  /*  end function prototypes */

  ses_error_flag check = check_errors_write_long_pFILE_xml(pFILE);
  if (check != SES_NO_ERROR) {
	return check;
  }

  ses_boolean return_value = SES_TRUE;

  // create and initialize memory
  
  const int BUFFER_LENGTH = 50;
  char* buffer = malloc(sizeof(char) * BUFFER_LENGTH);
  int i = 0;
  for (i = 0; i < BUFFER_LENGTH; i++) {
    buffer[i] = ' ';
  }
  
  //  put the long into a string

  int length = 0;
  length = sprintf(buffer, "%ld", the_long);

  //  write the string to the FILE*

  for (i = 0; i < strlen(buffer); i++) {
    _write_char(pFILE, buffer[i]);
  }

  //  free memory and return

  free(buffer);
  buffer = (char*)NULL;

  return return_value;
}
ses_error_flag check_errors_write_long_pFILE_xml(FILE* pFILE) {

  if (pFILE == (FILE*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  return SES_NO_ERROR;
}

/************************************************/

long               _read_long_xml(struct _ses_file_handle* pSFH){

  /*  function prototypes */

  ses_error_flag check_errors_read_long_xml(struct _ses_file_handle* pSFH);

  /*  end function prototypes */

  ses_error_flag check = check_errors_read_long_xml(pSFH);
  if (check != SES_NO_ERROR) {
	return 0;
  }

  long return_value = 0;

  double value = _read_double_xml(pSFH, 0, SES_TRUE);

  return_value = (long)value;

  return return_value;

}

ses_error_flag check_errors_read_long_xml(struct _ses_file_handle* pSFH) {

  if (pSFH == (struct _ses_file_handle*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  if (pSFH->_c_file_handle == (FILE*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  return SES_NO_ERROR;
}

/************************************************/

ses_boolean _write_long_xml(struct _ses_file_handle* pSFH, long the_long) {

  /*  function prototypes */

  ses_error_flag check_errors_write_long_xml(struct _ses_file_handle* pSFH, long the_long);

  /*  end function prototypes */

  ses_error_flag check = check_errors_write_long_xml(pSFH, the_long);
  if (check != SES_NO_ERROR) {
	return check;
  }

  ses_boolean return_value = SES_TRUE;

  FILE* pFILE = pSFH->_c_file_handle;
  return_value = _write_long_pFILE_xml(pFILE, the_long);
 
  return return_value;
}

ses_error_flag check_errors_write_long_xml(struct _ses_file_handle* pSFH, long the_long) {

  if (pSFH == (struct _ses_file_handle*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  if (pSFH->_c_file_handle == (FILE*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  return SES_NO_ERROR;
}



/************************************************/

double            _read_double_pFILE_xml(FILE* pFILE) {

  /*  function prototypes */

  ses_error_flag check_errors_read_double_pFILE_xml(FILE* pFILE);

  /*  end function prototypes */

  ses_error_flag check = check_errors_read_double_pFILE_xml(pFILE);
  if (check != SES_NO_ERROR) {
	return 0.0;
  }

  double return_value = 0.0;

  /*  read the double as chars from the file */
  
  const int BUFFER_LENGTH = 15;
#ifndef _MSC_VER
  char myBuffer[BUFFER_LENGTH];  
#else
  char myBuffer[15];  
#endif

  char cbuffer = ' ';

  /*  peel off leading blanks */

  int number_read = fread(&cbuffer, 1, 1, pFILE);
  while((cbuffer == ' ') && (number_read > 0)) {
     number_read = fread(&cbuffer, 1, 1, pFILE);
  }

  /*  put the text into a string */

  int index = 0;
  while ((cbuffer != ' ') && (number_read > 0)) {
      myBuffer[index] = cbuffer;
      index++;
      number_read = fread(&cbuffer, 1, 1, pFILE);
    
  }
   
  myBuffer[index] = '\0';

  /* convert the input character array to a double  */

  double mydBuffer = 0.0;

  char* the_string = &myBuffer[0];
  mydBuffer = strtod(the_string, NULL);
  return_value = mydBuffer;
 
  return return_value;
  
}

ses_error_flag check_errors_read_double_pFILE_xml(FILE* pFILE) {

  if (pFILE == (FILE*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  return SES_NO_ERROR;
}

/************************************************/

ses_boolean _write_double_pFILE_xml(FILE* pFILE, double the_double) {

  /*  function prototypes */

  ses_error_flag check_errors_write_double_pFILE_xml(FILE* pFILE);

  /*  end function prototypes */

  ses_error_flag check = check_errors_write_double_pFILE_xml(pFILE);
  if (check != SES_NO_ERROR) {
	return check;
  }

  ses_boolean return_value = SES_TRUE;
  const int BUFFER_LENGTH = 50;

  // create and initialize memory

  char* buffer = malloc(sizeof(char) * BUFFER_LENGTH);
  int i = 0;
  for (i = 0; i < BUFFER_LENGTH; i++) {
    buffer[i] = ' ';
  }

  //  write double into a string
  int length = 0;
  length = sprintf(buffer, "%e ", the_double);

  //  write out the string

  for (i = 0; i < strlen(buffer); i++) {
    _write_char(pFILE, buffer[i]);
  }

  //  free memory and return

  free(buffer);
  buffer = (char*)NULL;

  return return_value;
}
ses_error_flag check_errors_write_double_pFILE_xml(FILE* pFILE) {

  if (pFILE == (FILE*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  return SES_NO_ERROR;
}


/************************************************/

double             _read_double_xml(struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_validation){

 /*  function prototypes */

  ses_error_flag check_errors_read_double_xml(struct _ses_file_handle* pSFH);

  /*  end function prototypes */

  ses_error_flag check = check_errors_read_double_xml(pSFH);
  if (check != SES_NO_ERROR) {
	return 0.0;
  }

  return _read_double_pFILE_xml(pSFH->_c_file_handle);

}
ses_error_flag check_errors_read_double_xml(struct _ses_file_handle* pSFH) {

  if (pSFH == (struct _ses_file_handle*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  if (pSFH->_c_file_handle == (FILE*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }

  return SES_NO_ERROR;
}

/*****************************************************/

ses_boolean        _write_double_xml(struct _ses_file_handle* pSFH, double the_double, unsigned int nsig, ses_boolean do_validation){

 /*  function prototypes */

  ses_error_flag check_errors_write_double_xml(struct _ses_file_handle* pSFH);

  /*  end function prototypes */

  ses_error_flag check = check_errors_write_double_xml(pSFH);
  if (check != SES_NO_ERROR) {
	return SES_FALSE;
  }

  return _write_double_pFILE_xml(pSFH->_c_file_handle, the_double);

}
ses_error_flag check_errors_write_double_xml(struct _ses_file_handle* pSFH) {

  if (pSFH == (struct _ses_file_handle*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  if (pSFH->_c_file_handle == (FILE*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }

  return SES_NO_ERROR;
}

/************************************************/


ses_word _read_word_pFILE_xml(FILE* pFILE) {

  /*  function prototypes */

  ses_error_flag check_errors_read_word_pFILE_xml(FILE* pFILE);

  /*  end function prototypes */

  ses_error_flag check = check_errors_read_word_pFILE_xml(pFILE);
  if (check != SES_NO_ERROR) {
	return check;
  }

  /*  read a word in text */

  ses_word return_value = 0;
  const int BUFFER_LENGTH = 100;
#ifndef _MSC_VER
  char buffer[BUFFER_LENGTH];
#else
  char buffer[100];
#endif

  /* from the current c file handle (pFILE), read a ses_word in text, convert to ses_word,
     and return it */

  /*  read a string in from the current file handle into the buffer (ending delimiter is blank space)*/

  char c = _read_char(pFILE);
  while ((c == ' ') || (c == '\n')) {
    c = _read_char(pFILE);
  }

  /*  after blank space  */

  int i = 0;
 
  while ((i < BUFFER_LENGTH - 10) && (c  != ' ')) {
    buffer[i] = c;
    i++;
    c = _read_char(pFILE);
  }
  buffer[i] = '\0';

  /*  convert the buffer to a ses_word */

  return_value = (ses_word)atof(&buffer[0]);

  return return_value;
}


ses_error_flag check_errors_read_word_pFILE_xml(FILE* pFILE) {

  if (pFILE == (FILE*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  return SES_NO_ERROR;
}

/************************************************/

//ses_boolean        _write_word_pFILE_xml(FILE* pFILE, ses_word the_word);

/************************************************/

ses_word_reference  _read_word_list_pFILE_xml(FILE* pFILE, int num) {

  /*  function prototypes */

  ses_error_flag check_errors_read_word_list_pFILE_xml(FILE* pFILE, int num);

  /*  end function prototypes */

  ses_error_flag check = check_errors_read_word_list_pFILE_xml(pFILE, num);
  if (check != SES_NO_ERROR) {
	return (ses_word_reference)NULL;
  }

  ses_word_reference return_value = malloc(sizeof(ses_word) * num);

  /*  the file handle is positioned at the start of the text array */

  /*  read the list */

  int i = 0;
  for (i = 0;  i < num; i++) {
    return_value[i] = _read_word_pFILE_xml(pFILE);
  }

  /*  the file handle is positioned at the end of the text array*/

  return return_value;
}

ses_error_flag check_errors_read_word_list_pFILE_xml(FILE* pFILE, int num) {

  if (pFILE == (FILE*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  if (num <= 0) {
	return SES_OBJECT_OUT_OF_RANGE;
  } 
  return SES_NO_ERROR;
}

/************************************************/
//ses_boolean        _write_word_list_pFILE_xml(FILE* pFILE, ses_word the_array[], int size_array);

//ses_word_reference _read_word_list_xml(struct _ses_file_handle* pSFH, int num);
//ses_boolean        _write_word_list_xml(struct _ses_file_handle* pSFH, ses_word the_array[], long size_array);

/****************************************/

ses_word_reference _read_ses_word_array_xml(struct _ses_file_handle* pSFH, long size_array, unsigned int nsig, ses_boolean do_validation) {

 /*  function prototypes */

  ses_error_flag check_errors_read_ses_word_array_xml(struct _ses_file_handle* pSFH, int size_array);

  /*  end function prototypes */

  ses_error_flag check = check_errors_read_ses_word_array_xml(pSFH, (int)size_array);
  if (check != SES_NO_ERROR) {
	return (ses_word_reference)NULL;
  }

  ses_word_reference return_value = malloc(sizeof(ses_word) * size_array);
  int i = 0;
  for (i = 0; i < size_array; i++) {
	return_value[i] = _read_double_xml(pSFH, nsig, do_validation);
  }
  
  
  return return_value;


}
ses_error_flag check_errors_read_ses_word_array_xml(struct _ses_file_handle* pSFH, int size_array) {

  if (pSFH == (struct _ses_file_handle*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  if (pSFH->_c_file_handle == (FILE*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  if (size_array <= 0) {
	return SES_OBJECT_OUT_OF_RANGE;
  } 
  return SES_NO_ERROR;
}

/***************************************/


ses_boolean        _write_ses_word_array_xml(struct _ses_file_handle* pSFH, ses_word the_array[], long size_array, unsigned int nsig, ses_boolean do_validation){

  /*  function prototypes */

  ses_error_flag check_errors_write_ses_word_array_xml(struct _ses_file_handle* pSFH, int size_array);

  /*  end function prototypes */

  ses_error_flag check = check_errors_write_ses_word_array_xml(pSFH, (int)size_array);
  if (check != SES_NO_ERROR) {
	return SES_FALSE;
  }

  FILE* pFILE = pSFH->_c_file_handle;
  int i = 0;
  ses_boolean didit_write = SES_NO_ERROR;
  for (i = 0; i < size_array; i++) {
	didit_write = _write_double_xml(pSFH, the_array[i], nsig, do_validation);
        if (i < size_array - 1) {
		_write_tag(pFILE, "  ");
	}
  }
  
  
  return SES_TRUE;
}

ses_error_flag check_errors_write_ses_word_array_xml(struct _ses_file_handle* pSFH, int size_array) {

  if (pSFH == (struct _ses_file_handle*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  if (pSFH->_c_file_handle == (FILE*)NULL) {
	return SES_NULL_OBJECT_ERROR;
  }
  if (size_array <= 0) {
	return SES_OBJECT_OUT_OF_RANGE;
  } 
  return SES_NO_ERROR;
}

/***************************************/

//ses_string         _read_ses_string_xml(struct _ses_file_handle* pSFH);

/***************************************/
ses_boolean        _write_ses_string_xml(struct _ses_file_handle* pSFH, ses_string the_string, int size_array){

 /*  function prototypes */

  ses_error_flag check_errors_write_ses_string_xml(struct _ses_file_handle* pSFH, ses_string the_string, int num);

  /*  end function prototypes */

  ses_error_flag check = check_errors_write_ses_string_xml(pSFH, the_string, size_array);
  if (check != SES_NO_ERROR) {
	return SES_FALSE;
  }

  /*  make sure is ended properly */

  the_string[size_array-1] = '\0';

  /*  write the string out */

  FILE* pFILE = pSFH->_c_file_handle;
  _write_tag(pFILE, the_string);

  return SES_TRUE;


}
ses_error_flag check_errors_write_ses_string_xml(struct _ses_file_handle* pSFH, ses_string the_string, int num) {

  if (pSFH == (struct _ses_file_handle*)NULL) {
    return SES_NULL_OBJECT_ERROR;
  }
  if (pSFH->_c_file_handle == (FILE*)NULL) {
    return SES_NULL_OBJECT_ERROR;
  }
  if (the_string == (ses_string)NULL) {
    return SES_NULL_OBJECT_ERROR;
  }
  if (num <= 0) {
    return SES_OBJECT_OUT_OF_RANGE;
  }
  return SES_NO_ERROR;
}
/***************************************/










#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <stdio.h>
#include <math.h>
#include <string.h>

#include "_file_list_ascii.h"

#undef DEBUG_PRINT

#define check_errors_READ_SES_WORD_ARRAY_ASCII HEADER(check_errors_READ_SES_WORD_ARRAY_ASCII)
#define my_parse_doubles_from_line HEADER(my_parse_doubles_from_line)

//////  defines /////////////////////
//  MAX_BUFFER_SIZE -- for using a fixed length string buffer
/////////////////////////////////////

#define MAX_BUFFER_SIZE 300

// _read_double_tomax_ascii reads in a "to max" number of doubles while skipping over the end of line cruft.
double _read_double_tomax_ascii(struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_validation, int max_length) {
    
    /*  read a double from the current file handle */
    /*  the current code reads ascii files */
    
    /*  function prototypes */
    
    double my_truncate_for_significant_digits(double the_double, unsigned intnsig);
    ses_boolean my_do_validation_double(double the_double);
    
    /*  end function prototypes */
#ifdef DEBUG_PRINT
    printf("_read_double_tomax_ascii: size: %d, nsig: %d, do_validation: %d\n",max_length, nsig, do_validation);
#endif
    
    double return_value = 0.0;
    
    /********************************************************************/
    /*  error check the arguments */
    
    if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
        printf("_read_double_tomax_ascii: Null ses file handle passed to _read_double_ascii\n");
#endif
        _set_latest_error(SES_NULL_OBJECT_ERROR);
        return return_value;
    }
    
    int lnsig = nsig;
    if (lnsig < 0) {
#ifdef DEBUG_PRINT
        printf("_read_double_tomax_ascii: nsig < 0, error -- changing nsig to 0\n");
#endif
        nsig = 0;
        
    }
    
    /*  internal error checking */
    
    FILE* pFILE = pSFH->_c_file_handle;
    if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
        printf("_read_double_tomax_ascii: Null FILE* in _read_double\n");
#endif
        _set_latest_error(SES_NULL_OBJECT_ERROR);
        return return_value;
    }
    /********************************************************************/
    
    /*  read the double as chars from the file */
    
    char myBuffer[MAX_BUFFER_SIZE];
    char cbuffer = ' ';
    
    
    int number_read = fread(&cbuffer, 1, 1, pFILE);
    int index       = 0;
    int tolength    = 1;
    
#ifdef DEBUG_PRINT
    printf("_read_double_tomax_ascii, FIRST read. number_read: %d, cbuffer: '%c'\n",number_read, cbuffer);
#endif
    
    //  peel off leading blanks
    while ((cbuffer == ' ') && (number_read > 0) && (tolength < max_length)) {
        myBuffer[index] = cbuffer;
        index++;
        number_read = fread(&cbuffer, 1, 1, pFILE);
#ifdef DEBUG_PRINT
            printf("Peel Off:: INDEX: %d, cbuffer: '%c'\n", index, cbuffer);
#endif
        tolength++;
    }
    
    //  read the double as a string
#ifdef DEBUG_PRINT
    printf("_read_double_tomax_ascii: max_length: %d, INDEX: %d cbuffer:", max_length, index);
#endif
  while ((number_read > 0) && (tolength < max_length)) {
    
    myBuffer[index] = cbuffer;
    index++;
    number_read = fread(&cbuffer, 1, 1, pFILE);
    
    // Some times the indexing is screwed up, if you hit \r or \n and the index=5, then you have
    // run into the end of line cruft (11111 to 10000), skip over that crap and start at new index & tolength.
    
    if ((cbuffer == '\r' || cbuffer == '\n') & (index == 5)){
#ifdef DEBUG_PRINT
      printf("\n_read_double_tomax_ascii INDEX is 5!: index: %d tolength: %d, max_length: %d\n", index, tolength, max_length);
#endif
      
      // Start at the beginning of the next line!?
      number_read = fread(&cbuffer, 1, 1, pFILE);
      index = 0;
      tolength = 0;
      
      //  peel off leading blank - there should be one, if it's a positive number.
      while ((cbuffer == ' ') && (number_read > 0) && (tolength < max_length)) {
        myBuffer[index] = cbuffer;
        index++;
        number_read = fread(&cbuffer, 1, 1, pFILE);
#ifdef DEBUG_PRINT
        printf("Peel:: INDEX: %d, cbuffer: '%c'\n", index, cbuffer);
#endif
        tolength++;
      }
    }
    
#ifdef DEBUG_PRINT
        printf(" %c",cbuffer);
#endif
       tolength++;
       
    }
#ifdef DEBUG_PRINT
    printf("\n");
    printf("_read_double_tomax_ascii SECOND Time: INDEX: %d tolength: %d  ", index, tolength);
#endif
    

#ifdef DEBUG_PRINT
    // Move the file pointer to te correct index:
    int current_location = ftell( pSFH->_c_file_handle);
    printf("_read_double_tomax_ascii: CURRENT_location: %d\n", current_location);
#endif
    
    if (tolength == max_length) {
        myBuffer[index] = cbuffer;
    }
    index++;
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
#ifdef DEBUG_PRINT
          printf("_read_double_tomax_ascii: from tmp_double.\n");
#endif
    }
    else {
        return_value = mydBuffer;
#ifdef DEBUG_PRINT
          printf("_read_double_tomax_ascii: from mydBuffer, return_value: %lf\n", return_value);
#endif
    }
    
    /*  do validation */
    
    if (do_validation == SES_TRUE) {
        if (my_do_validation_double(return_value) == SES_FALSE) {
#ifdef DEBUG_PRINT
            printf("_read_double_tomax_ascii: do validation error on double -- setting error\n");
#endif
            _set_latest_error(SES_READ_ERROR);
            return_value = 0.0;
        }
    }
#ifdef DEBUG_PRINT
    printf("_read_double_tomax_ascii: value: %lf\n",return_value);
#endif
    
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
    
    ses_ascii_word_size word_size = 0;
    
    word_size = pSFH->_word_size;
#ifdef DEBUG_PRINT
    printf("_write_double_ascii:  word_size: %d\n", word_size);
#endif
    
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
#ifdef DEBUG_PRINT
    int the_size1 = 0;
#endif    
    if (word_size == ASCII_SMALL_WORD_SIZE){
#ifdef DEBUG_PRINT
        the_size1 =
#endif
	  sprintf(format," %2d.8E", size);
    }
    else{
#ifdef DEBUG_PRINT
        the_size1 =
#endif
	  sprintf(format," %2d.15E", size);
    }
#ifdef DEBUG_PRINT
    printf("_write_double_ascii:  the_size1: %d\n", the_size1);
#endif
    
    format[0] = '%';
    format[7] = '\0';
    
    int the_size = snprintf(the_string, MAX_BUFFER_SIZE, &format[0], local_double);
    the_string[the_size] = '\0';
    /*  write the string */
    
    /* int number_written = 0; */
    
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
            /* number_written = 0; */
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
                /* number_written = */ fwrite(&the_string[0], 1, the_size, pFILE);
                
            }
            else {
                /* number_written = */ fwrite(&the_string[0], 1, the_size, pFILE);
            }
            
            free(number);
            free(exponent);
        }
        
    }
    else {
        /* number_written = 0; */
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

long  _read_long_tomax_ascii(struct _ses_file_handle* pSFH, unsigned int fixed_size) {
    
    /*  read a long as an ascii from the current file handle */
    
    long return_value;
    
    /********************************************************************/
    /*  error check the arguments */
    
    if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
        printf("_read_long_tomax_ascii: ses file handle null in _read_long\n");
#endif
        _set_latest_error(SES_NULL_OBJECT_ERROR);
        return 0;
    }
    
    FILE* pFILE = pSFH->_c_file_handle;
    
    if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
        printf("_read_long_tomax_ascii: null c file handle into _read_long_ascii\n");
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
    
    int index = 0;
    int number_read = fread(&cbuffer, 1, 1, pFILE);
    myBuffer[index] = cbuffer;
    index = 1;

#ifdef DEBUG_PRINT
    printf("_read_long_tomax_ascii1: Index: %d cbuffer: \'%c\'\n", index, cbuffer);
#endif
    
    
    //  while ( (((int)cbuffer != 32) || (at_start == SES_TRUE)) && (number_read > 0)) {
    while ( (((int)cbuffer != 32) || (at_start == SES_TRUE)) && (number_read > 0) && (index < fixed_size)) {
        number_read = fread(&cbuffer, 1, 1, pFILE);
        myBuffer[index] = cbuffer;
        index++;
#ifdef DEBUG_PRINT
        printf("_read_long_tomax_ascii:  Index: %d,  cbuffer: %c, index %d\n", index, cbuffer, index);
#endif
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
    
#ifdef DEBUG_PRINT
    printf("_read_long_tomax_ascii:  mylBuffer: %ld, fixed_size: %i\n", mylBuffer, fixed_size);
#endif
    
    if (index <= 0) {
#ifdef DEBUG_PRINT
        printf("_read_long_tomax_ascii: Failure in reading long, fixed_size: %i\n", fixed_size);
#endif
        _set_latest_error(SES_READ_ERROR);
        return_value = 0;
    }
    else {
        return_value = mylBuffer;
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


long  _read_long_tomax_pFILE_ascii(FILE* pFILE, unsigned int fixed_size, ses_boolean needs_flip) {
    
    /*  read a long from the current file handle */
    
    long return_value;
    
    /*  error check the arguments */
    
    
    /********************************************************************/
    if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
        printf("_read_long_tomax_pFILE_ascii: null c file handle into _read_long_pFILE_ascii\n");
#endif
        _set_latest_error(SES_NULL_OBJECT_ERROR);
        return 0;
    }
    /********************************************************************/
    
    /*  read the long as a character string from the file */
    
    long mylBuffer;
    char myBuffer[MAX_BUFFER_SIZE];
    char cbuffer = ' ';
    int number_read = fread(&cbuffer, 1, 1, pFILE);
    int index       = 0;
    int tolength    = 1;
    
#ifdef DEBUG_PRINT
    printf("_read_long_tomax_pFILE_ascii: index: %d, cbuffer: %c, number_read: %d\n",index, cbuffer, number_read);
#endif
    //  peel off leading blanks
    while ((cbuffer == ' ') && (number_read > 0) && (tolength < fixed_size)) {
        number_read = fread(&cbuffer, 1, 1, pFILE);
        tolength++;
#ifdef DEBUG_PRINT
        printf("_read_long_tomax_pFILE_ascii: tolength: %d, cbuffer: %c\n",tolength, cbuffer);
#endif
    }
    // Grab the first non-blank character
    myBuffer[index] = cbuffer;
    index++;
#ifdef DEBUG_PRINT
    printf("_read_long_tomax_pFILE_ascii: myBuffer: %s\n", myBuffer);
#endif
    
    // read the actual value
    while ((number_read > 0) && (tolength < fixed_size)) {
        
        number_read = fread(&cbuffer, 1, 1, pFILE);
        tolength++;
        
        myBuffer[index] = cbuffer;
        index++;

#ifdef DEBUG_PRINT
        printf("_read_long_tomax_pFILE_ascii: tolength: %d, index: %d, cbuffer: %c\n", tolength, index, cbuffer);
#endif
    }
    
    myBuffer[index] = '\0';
    char* the_string = &myBuffer[0];
#ifdef DEBUG_PRINT
    printf("_read_long_tomax_pFILE_ascii: index: %d, myBuffer %s, the_string: %s\n",index, myBuffer, the_string);
#endif
    
    /* convert the input character stream to a long  */
    mylBuffer = 0;
    
    double myldBuffer = strtod(the_string, NULL);
    mylBuffer = (long)myldBuffer;
    
#ifdef DEBUG_PRINT
    printf("_read_long_tomax_pFILE_ascii:  read long, mylBuffer: %ld, index: %i\n", mylBuffer,index);
#endif
    
    
    if (index < 0) {
#ifdef DEBUG_PRINT
        printf("_read_long_tomax_pFILE_ascii: Failure in reading long\n");
#endif
        _set_latest_error(SES_READ_ERROR);
        return_value = 0;
    }
    else {
        return_value = mylBuffer;
    }
    
    return return_value;
    
}


long _read_long_pFILE_ascii(FILE* pFILE, ses_boolean needs_flip) {
    
    /*  read a long from the current file handle */
    
    long return_value = 0;
    
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
    
    int last_non_zero = strlen(line);
    for (i = strlen(line); i >= 0; i--) {
        if ((line[i] != ' ') && (line[i] != '\n') && (line[i] != '\0')) break;
    }
    last_non_zero = i;
    
    int number_elements = ((last_non_zero+2)/size_double);
    start_index = 0;
    
    char* pEnd;
    for (i = 0; i < number_elements; i++) {
        
        if (array_index < size_array) {
            
            if (size_double > 0) {
                the_double_as_string = malloc(sizeof(char)*(size_double+1));
                if (the_double_as_string != (char*)NULL) {
                    strncpy(the_double_as_string, &line[start_index], size_double);
                    the_double_as_string[size_double] = '\0';
                    mydBuffer = strtod(the_double_as_string, &pEnd);
                    
                    the_array[array_index] = mydBuffer;
                    
                    free(the_double_as_string);
                    the_double_as_string = (char*)NULL;
                }
            }
            
        }
        
        start_index = start_index + size_double;
        array_index++;
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
#ifdef DEBUG_PRINT
    printf("_read_ses_word_array_ascii: size of read: %ld\n", size_array);
#endif
    
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
    
#ifdef DEBUG_PRINT
    printf("_read_ses_word_array_ascii: size of read: %ld, word_size: %d\n", size_array, pSFH->_word_size);
#endif
    
    int    num_lines        = 0;
    int    current_location = ftell(pSFH->_c_file_handle);
    double tmp              = _read_double_tomax_ascii(pSFH, nsig, do_validation,pSFH->_word_size);
#ifdef DEBUG_PRINT
    int    next_location    = ftell(pSFH->_c_file_handle);
#endif
    
    if (size_array == 1) {
        return_value[0] = tmp;
        
#ifdef DEBUG_PRINT
        int diff_locations = next_location - current_location;
        printf("_read_ses_word_array_ascii: next_LOCATION: %d, current_location: %d, difference: %d\n", next_location, current_location, diff_locations);
#endif
        
        return return_value;
    }
    
    
#ifdef DEBUG_PRINT
    printf("_read_ses_word_array_ascii: next_location: %d, current_location: %d, pSFH->_word_size: %d\n", next_location, current_location, pSFH->_word_size);
    printf("_read_ses_word_array_ascii: _word_size SET to %d, tmp : %f\n", pSFH->_word_size, tmp);
#endif
    
    fseek(pSFH->_c_file_handle, current_location, SEEK_SET);
    
    int i = 0;
    char line[128];
    
    /*  initialize */
    for (i=0; i < 127; i++) {
        line[i] = ' ';
    }
    line[127] = '\0';
    
    int num_per_line = (128/pSFH->_word_size);
    
    num_lines = size_array/num_per_line;
    if ( (num_lines * num_per_line) <= 0) {
        num_lines++;    //  gotta be at least one line
    }
    
    int j = 0;
    int start_index = 0;
    int double_index = 0;
    for (i=0; i < num_lines; i++) {
        current_location = ftell(pSFH->_c_file_handle);
        fgets ( &line[0], sizeof line, pSFH->_c_file_handle);
        //printf("_read_ses_word_array_ascii: LINE \'%s\', num_lines: %d\n", line, num_lines);
        for (j = 127; j >= 0; j--) {
            if (line[j] == '1') break;
        }
        
        start_index = j - 4;
        if (start_index < 0) {
            start_index = 122;
        }
        
        //  strip the crap off the end of the line
        if (start_index > 0) {
            int k = 0;
            for (k = start_index; k < 128; k++) {
                line[k] = ' ';
            }
            line[127] = '\0';
        }
        //  parse the doubles off the line
        int next_index = my_parse_doubles_from_line(&line[0], return_value, double_index, pSFH->_word_size, size_array, pSFH->_c_file_handle);
        
        double_index = next_index;
        if ((i == num_lines-1) && (next_index < size_array)) {
            num_lines++;
        }
        if (next_index >= size_array) {
            //  on the last line, you've read some of the 'next' array
            //  need to move the file handle back
            int num_on_line = (ftell(pSFH->_c_file_handle) - current_location)/pSFH->_word_size;
            
#ifdef DEBUG_PRINT
            printf("_read_ses_word_array_ascii: Num_on_line %d\n", num_on_line);
#endif
            
            fseek(pSFH->_c_file_handle, current_location , SEEK_SET);
            int i = 0;
            int many = num_on_line - (next_index - size_array)  ;
            for (i = 0; i < many; i++) {
#ifdef DEBUG_PRINT
                printf("_read_ses_word_array_ascii:  word_size: %d\n", pSFH->_word_size);
#endif
                tmp = _read_double_tomax_ascii(pSFH, nsig, do_validation, pSFH->_word_size);
            }
            
        }
        
    }
    
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
    
    
    return (ses_word_reference)SES_NO_ERROR;
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
            /* ses_boolean didit = */ _write_char_ascii(pSFH->_c_file_handle, '1');
            /* didit = */ _write_char_ascii(pSFH->_c_file_handle, '1');
            /* didit = */ _write_char_ascii(pSFH->_c_file_handle, '1');
            /* didit = */ _write_char_ascii(pSFH->_c_file_handle, '1');
            /* didit = */ _write_char_ascii(pSFH->_c_file_handle, '1');
            /* didit = */ _write_char_ascii(pSFH->_c_file_handle, '\n');
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






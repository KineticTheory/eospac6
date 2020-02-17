

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <stdio.h>
#include <math.h>

#include "_file_list_binary.h"

#undef DEBUG_READ_LONG_PFILE_BINARY

double _read_double_binary(struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_validation) {

  /*  read a long from the current file handle */
  /*  the current code reads little endian files */

  /*  function prototypes */

  double my_flip_bytes(double the_double);
  double my_truncate_for_significant_digits(double the_double, unsigned intnsig);
  ses_boolean my_do_validation_double(double the_double);

  /*  end function prototypes */

  double return_value = 0.0;

  /********************************************************************/
  /*  error check the arguments */

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_double_binary: Null ses file handle passed to _read_double\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return return_value;
  }


  /*  internal error checking */

  FILE* pFILE = _getPFILE(pSFH);
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_double_binary: Null FILE* in _read_double\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return return_value;
  }

  int lnsig = nsig;
  if (lnsig < 0) {
#ifdef DEBUG_PRINT
    printf("_read_double_binary: nsig < 0, error -- changing nsig to 0\n");
#endif
    nsig = 0;
    
  }
  /********************************************************************/
  

  /*  set the byte flipping flag */

  
  ses_boolean needs_flip = SES_FALSE;
  if (pSFH->_needs_flip == SES_TRUE) {
    needs_flip = SES_TRUE;
  }

  union buffer {

    char cbuffer[8];
    double dbuffer;
  } myBuffer;

  int number_read = fread(&myBuffer.cbuffer[0], 8, 1, pFILE);
  if (ferror(pFILE))
    printf("Read error: __FILE__ (__LINE__)");
  if (feof(pFILE))
    printf("End-of-File error:  __FILE__ (__LINE__)");
  if (number_read <= 0) {
#ifdef DEBUG_PRINT
    printf("_read_double_binary: Failure in reading double from pFILE at location %ld\n", ftell(pFILE));
#endif
    _set_latest_error(SES_READ_ERROR);
    return_value = 0.0;
  }
  else {

    /*  flip the bits */

    double tmp = 0.0;
    if (needs_flip == SES_TRUE) {
      tmp = my_flip_bytes(myBuffer.dbuffer);
      myBuffer.dbuffer = tmp;
    }

    /*  truncate for significant digits */

    double tmp_double;
    if (nsig != 0) {
      tmp_double = my_truncate_for_significant_digits(myBuffer.dbuffer, nsig);
      return_value = tmp_double;
    }
    else {
      return_value = myBuffer.dbuffer;
    }

    /*  do validation */

    if (do_validation == SES_TRUE) {
      if (my_do_validation_double(return_value) == SES_FALSE) {
#ifdef DEBUG_PRINT
        printf("_read_double_binary: do validation error on double -- setting error\n");
#endif
        _set_latest_error(SES_READ_ERROR);
        return_value = 0.0;
      }
    }
  }

  //_releasePFILE(pSFH);  

  return return_value;


}


ses_boolean _write_double_binary(struct _ses_file_handle* pSFH, double the_double, unsigned int nsig, ses_boolean do_validation) {

  /*  write aa double to the current file handle */
  /*  the current code writes little endian files */

  /*  function prototypes */

  double my_truncate_for_significant_digits(double the_double, unsigned intnsig);
  ses_boolean my_do_validation_double(double the_double);

  /*  end function prototypes */

  ses_boolean return_value = SES_FALSE;

  /********************************************************************/
  /*error check the arguments */

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_double_binary:  null ses file handle into _write_double\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  int lnsig;
  lnsig = (int)nsig;
  if (lnsig < 0) {
#ifdef DEBUG_PRINT
    printf("_write_double: nsig < 0, changing to 0 (no truncation)\n");
#endif
    nsig = 0;
  }
  

   ses_boolean need_to_release = SES_FALSE;  
   FILE* pFILE = pSFH->_c_file_handle;
   if (pSFH->_c_file_handle == (FILE*)NULL) {
        pFILE = _getPFILE(pSFH);
        need_to_release = SES_TRUE;
  }

 
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_double_binary: Null FILE* in _write_double\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return return_value;
  }

  /********************************************************************/

  union buffer {
    char cbuffer[8];
    double dbuffer;
  } myBuffer;

  if (do_validation == SES_TRUE) {
      if (my_do_validation_double(the_double) == SES_FALSE) {
#ifdef DEBUG_PRINT
        printf("_write_double: do validation error on double -- setting error\n");
#endif
        _set_latest_error(SES_WRITE_ERROR);
        return_value = SES_FALSE;
      }
    
  }

  double the_trunc = 0.0;
  the_trunc =  my_truncate_for_significant_digits(the_double, nsig);

  myBuffer.cbuffer[0] = 0;
  myBuffer.cbuffer[1] = 0;
  myBuffer.cbuffer[2] = 0;
  myBuffer.cbuffer[3] = 0;
  myBuffer.cbuffer[4] = 0;
  myBuffer.cbuffer[5] = 0;
  myBuffer.cbuffer[6] = 0;
  myBuffer.cbuffer[7] = 0;

  myBuffer.dbuffer = the_trunc;

  /*  set flipping flag */

  ses_boolean needs_flip = SES_FALSE;
  if (pSFH->_needs_flip) {
    needs_flip = SES_TRUE;
  }

  if (needs_flip == SES_TRUE) {
    myBuffer.dbuffer = my_flip_bytes(myBuffer.dbuffer);
  }


  int number_written = fwrite(&myBuffer.cbuffer[0], 8, 1, pFILE);
  if (number_written <= 0) {
#ifdef DEBUG_PRINT
    printf("_write_double: Failure in writing double to pFILE\n");
#endif
    _set_latest_error(SES_WRITE_ERROR);
    return_value = SES_FALSE;
  }
  else {
    return_value = SES_TRUE;
  }

  if (need_to_release == SES_TRUE) {
	_releasePFILE(pSFH);
  }
  return return_value;


}





long _read_long_binary(struct _ses_file_handle* pSFH) {

  /*  read a long as a double from the current file handle */
  /*  the current code reads little endian files */

  /*  function prototypes */

  double my_flip_bytes(double the_double);

  /*  end function prototypes */

  long return_value = 0;


  /********************************************************************/
  /*  error check the arguments */

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_long_binary: ses file handle null in _read_long\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  FILE* pFILE = pSFH->_c_file_handle;
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_long_binary: Null FILE* in _read_long\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return return_value;
  }
  /********************************************************************/
  ses_boolean needs_flip = SES_FALSE;
  if (pSFH->_needs_flip == SES_TRUE) {
    needs_flip = SES_TRUE;
  }

  union buffer {
    char cbuffer[8];
    double dbuffer;
  } myBuffer;

  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_long_binary: null c file handle into _read_long\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  int number_read = fread(&myBuffer.cbuffer[0], 8, 1, pFILE);
  if (ferror(pFILE))
    printf("Read error: __FILE__ (__LINE__)");
  if (feof(pFILE))
    printf("End-of-File error:  __FILE__ (__LINE__)");
  if (number_read <= 0) {
#ifdef DEBUG_PRINT
    printf("_read_long_binary: Failure in reading long from pFILE\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return_value = 0;
  }
  else {

    double tmp = 0.0;
    if (needs_flip == SES_TRUE) {
      tmp = my_flip_bytes(myBuffer.dbuffer);
      return_value = tmp;
    }
    else {
      return_value = myBuffer.dbuffer;
    }
  }

  return return_value;


}



ses_boolean _write_long_binary(struct _ses_file_handle* pSFH, long the_long) {

  /*  write a long as a double to the current file handle */
  /*  the current code writes little endian files */

  ses_boolean return_value = SES_FALSE;
  /********************************************************************/

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_long_binary: null ses file handle into _write_long\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  FILE* pFILE = pSFH->_c_file_handle;
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_long_binary: Null FILE* in _write_long\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return return_value;
  }
  /********************************************************************/

  /*  set flipping flag */

  ses_boolean needs_flip = SES_FALSE;
  if (pSFH->_needs_flip == SES_TRUE) {
    needs_flip = SES_TRUE;
  }

  union buffer {
    char cbuffer[8];
    double dbuffer;
  } myBuffer;

  myBuffer.dbuffer = (double)the_long;

  if (needs_flip == SES_TRUE) {
    myBuffer.dbuffer = my_flip_bytes(myBuffer.dbuffer);
  }


  int number_written = fwrite(&myBuffer.cbuffer[0], 8, 1, pFILE);
  if (number_written <= 0) {
#ifdef DEBUG_PRINT
    printf("_write_long: Failure in writing long to pFILE\n");
#endif
    _set_latest_error(SES_WRITE_ERROR);
    return_value = SES_FALSE;
  }
  else {
    return_value = SES_TRUE;
  }


  return return_value;


}
long _read_long_pFILE_binary(FILE* pFILE, ses_boolean needs_flip) {

  /*  read a long as a double from the current file handle */

  long return_value = 0;

 /********************************************************************/
 /*  error check the arguments */

  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_long_pFILE_binary: null c file handle into _read_long_pFILE\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (long)0;
  }
 /********************************************************************/

  union buffer {
    char cbuffer[8];
    double dbuffer;
  } myBuffer;

#ifdef DEBUG_READ_LONG_PFILE_BINARY
   printf("reading long pfile binary location is %d\n", ftell(pFILE));
#endif

  int number_read = fread(&myBuffer.cbuffer[0], 8, 1, pFILE);
  if (ferror(pFILE))
    printf("Read error: __FILE__ (__LINE__)");
  if (feof(pFILE))
    printf("End-of-File error:  __FILE__ (__LINE__)");
#ifdef DEBUG_READ_LONG_PFILE_BINARY
  printf("_read_long_pFILE_binary:  number_read is %d\n", number_read);
#endif

  if (number_read <= 0) {
#ifdef DEBUG_PRINT
    printf("_read_long_pFILE_binary: Failure in reading long from pFILE\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return_value = (long)0;
  }
  else {


    double tmp = 0.0;
    if (needs_flip == SES_TRUE) {
      tmp = my_flip_bytes(myBuffer.dbuffer);
      return_value = (long)tmp;
    }
    else {
      return_value = (long)myBuffer.dbuffer;
    }
#ifdef DEBUG_READ_LONG_PFILE_BINARY
  printf("_read_long_pFILE_binary:  myBuffer.dbuffer is %e\n", myBuffer.dbuffer);
  printf("_read_long_pFILE_binary:  tmp is %e\n", tmp);
  printf("_read_long_pFILE_binary:  return_value is %ld\n", return_value);	
#endif

  }
  return return_value;


}

ses_boolean  _write_long_pFILE_binary(FILE* pFILE, ses_boolean needs_flip) {
  return SES_FALSE;
}

char _read_char_binary(FILE* pFILE ) {
	return _read_char(pFILE);
}

ses_boolean _write_char_binary(FILE* pFILE, char the_char) {
	return _write_char(pFILE, the_char);
}

ses_word_reference _read_ses_word_array_binary(struct _ses_file_handle* pSFH, long size_array, unsigned int nsig, ses_boolean do_validation) {


  /*  read a ses word array from the current file handle */

  ses_word_reference return_value = (ses_word_reference)NULL;

  /********************************************************************/
  /*  error check the arguments */

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_ses_word_array: ses file handle null in _read_ses_word_array\n");
#endif
    return (ses_word_reference)NULL;
  }


  if (size_array <= 0) {
#ifdef DEBUG_PRINT
    printf("_read_ses_word_array: size array <= 0\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (ses_word_reference)NULL;
  }
 /********************************************************************/

  /*  set flipping flag */


  /* ses_boolean needs_flip = SES_FALSE; */
  /* if (pSFH->_needs_flip == SES_TRUE) { */
  /*   needs_flip = SES_TRUE; */
  /* } */


  /*  allocate the memory for the return */

  return_value = malloc(sizeof(ses_word)*size_array);
  if (return_value == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_ses_word_array: memory allocation error\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (ses_word_reference)NULL;
  }


  int i=0;
  for(i=0; i<size_array; i++) {
    return_value[i] = _read_double(pSFH, nsig, do_validation);
  }

  return return_value;
}


ses_boolean _write_ses_word_array_binary(struct _ses_file_handle* pSFH, ses_word the_array[], long size_array, unsigned int nsig, ses_boolean do_validation) {

  /*  write an array of ses_word's to the current file handle */

  ses_boolean return_value = SES_TRUE;

  /********************************************************************/
  /*  error check the arguments */

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_ses_word_array_binary: null ses file handle\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (size_array <= 0) {
#ifdef DEBUG_PRINT
    printf("_write_ses_word_array_binary: size array <= 0\n");
#endif
    _set_latest_error(SES_ARRAY_SIZE_ERROR);
    return SES_FALSE;
  }
  /********************************************************************/

  /*  write the array */

  long i = 0;
  ses_boolean tmp = SES_TRUE;

  for (i=0; i < size_array; i++) {

    tmp = _write_double_binary(pSFH, (double)the_array[i], nsig, do_validation);
    if (tmp == SES_FALSE) {
#ifdef DEBUG_PRINT
      printf("_write_ses_word_array: _write_double_binary failed\n");
#endif
      _set_latest_error(SES_WRITE_ERROR);
      return SES_FALSE;
    }

  }

  return return_value;
}

ses_boolean        _write_ses_string_binary(struct _ses_file_handle* pSFH, ses_string the_string, long size_array){
  return SES_FALSE;
}



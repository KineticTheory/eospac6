

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <math.h>

/*    This file contains all functions that READ and WRITE 'little' objects (double, long, char, ses_material_id, ses_table_id, ses_word_array)
*/

/***************************************************************/

double             _read_double(struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_validation) {

  if (pSFH == (struct _ses_file_handle*)NULL) {
    return 0.0;
  }
  return pSFH->pt2_read_double(pSFH, nsig, do_validation);
}

/***************************************************************/

ses_boolean        _write_double(struct _ses_file_handle* pSFH, double the_double, unsigned int nsig, ses_boolean do_validation){

  if (pSFH == (struct _ses_file_handle*)NULL) {
     return SES_FALSE;
  }
  return pSFH->pt2_write_double(pSFH, the_double, nsig, do_validation);
}

/***************************************************************/


long _read_long(struct _ses_file_handle* pSFH) {

  if (pSFH == (struct _ses_file_handle*)NULL) {
     return 0;
  }
  return pSFH->pt2_read_long(pSFH);
}

/***************************************************************/

ses_boolean _write_long(struct _ses_file_handle* pSFH, long the_long) {

  if (pSFH == (struct _ses_file_handle*)NULL) {
     return SES_FALSE;
  }
  return pSFH->pt2_write_long(pSFH, the_long);
}

/***************************************************************/


ses_word_reference _read_ses_word_array(struct _ses_file_handle* pSFH, long size_array, unsigned int nsig, ses_boolean do_validation){

  if (pSFH == (struct _ses_file_handle*)NULL) {
    return (ses_word_reference)NULL;
  }
  return pSFH->pt2_read_ses_word_array(pSFH, size_array, nsig, do_validation);

}
/***************************************************************/


ses_boolean        _write_ses_word_array(struct _ses_file_handle* pSFH, ses_word the_array[], long size_array, unsigned int nsig, ses_boolean do_validation){

  if (pSFH == (struct _ses_file_handle*)NULL) {
     return SES_FALSE;
  }
  ses_boolean return_value = pSFH->pt2_write_ses_word_array(pSFH, the_array, size_array, nsig, do_validation);
  return return_value;

}
/***************************************************************/


char _read_char(FILE* pFILE ) {

  /*  read a char from the current c file handle */

  char return_value = (char)0;

  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_char: null c file handle into _read_char\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  char tmp = (char)0;
  int number_read = fread(&tmp, 1, 1, pFILE);
  if (number_read <= 0) {
    _set_latest_error(SES_READ_ERROR);
    return_value = 0;
  }
  else {
    return_value = tmp;
  }

  return return_value;
}

/***************************************************************/


ses_boolean _write_char(FILE* pFILE, char the_char) {

  /*  write a char to the current file handle */
  /*  note:  char's do NOT need flipping */

  ses_boolean return_value = SES_FALSE;

  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_char: null c file handle into _write_char\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  int number_written = fwrite(&the_char, 1, 1, pFILE);
  if (number_written <= 0) {
#ifdef DEBUG_PRINT
    printf("_write_char: Failure in writing char to pFILE\n");
#endif
    _set_latest_error(SES_WRITE_ERROR);
    return_value = SES_FALSE;
  }
  else {
    return_value = SES_TRUE;
  }


  return return_value;


}
/***************************************************************/



double my_get_mantissa(double x, int* exponent) {
 
  /*  return the mantissa and exponent of the double */
  /*  this routine requires exponent to already have allocated memory */

  double return_value = 0.0;
 
  /********************************************************************/
  /* error check the arguments */

  if (exponent == (int*)NULL) {
#ifdef DEBUG_PRINT
    printf("my_get_mantissa: exponent passed into my_get_mantissa without memory allocated\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return 0.0;
  }
  /********************************************************************/

  /*  get the mantissa */

  int i=0;
  int sign = 1;
  int exp_sign = 1;

  /*  get the absolute value of x */

  if (x < 0.0) {
    sign = -1;
    x = -1.0 * x;
  }

  /*  move the decimal point until you get the mantissa */

  double tmp = x;

  if (tmp < 1.0) {
     /*  0.0 <= tmp < 1.0 */
     if (tmp < 0.1) {
      /* 0.0 <= tmp < 0.1 */
     exp_sign = -1;
      while (tmp < 0.1 && tmp > 0.0) {
        tmp = tmp*10.0;
        i++;
      }
    }
    else {
      /*  0.1 <= tmp < 1.0  */
    }
  }
  else {
    /*  tmp >= 1.0  */

    while (tmp >= 1.0) {
      tmp = tmp/10.0;
      i++;
    }

  }

  return_value = tmp*sign;
  int the_exponent = i*exp_sign;

  *exponent = the_exponent;

  return return_value;

}
/***************************************************************/

double my_trunc_to_sig_dig(double x, unsigned int nsig, int* returned_sig) {

  /*  truncate the double to nsig significant digits */
  /*  if nsig == 0 do not truncates */
  /*  this routine assumes that returned_sig HAS memory */

  /*  function prototypes */

  double my_get_mantissa(double x, int* exponent);

  /*  function prototypes */

  double return_value = 0.0;


  /********************************************************************/
  /*  error check the arguments */

  if (nsig == 0) {
    return x;
  }


  if (returned_sig == (int*)NULL) {
#ifdef DEBUG_PRINT
    printf("my_trunc_to_sig_dig: returned_sig passed into my_trunc_to_sig_dig without memory allocated\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return 0.0;
  }
  /********************************************************************/

  /*  get the mantissa and exponent of x */

  int* exponent = malloc(sizeof(int)*1);
  if (exponent == (int*)NULL) {
#ifdef DEBUG_PRINT
    printf("my_trunc_to_sig_dig: malloc failed in my_trunc_to_sig_dig\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return 0.0;
  }

  double mantissa = my_get_mantissa(x, exponent);

  /*  truncate the mantissa as a long to nsig significant digits */

  long trunced = (long)(mantissa * pow(10.0,nsig*1.0));

  /*  compute the double from the truncated mantissa */
 
  double almost_there = 0.0;
 
  if (((int)nsig - exponent[0]) < 0) {
    almost_there  = (double)trunced * pow(10.0, ((int)nsig-exponent[0])*-1.0);
  }
  else {
    almost_there  = (double)trunced * pow(10.0, ((int)nsig-exponent[0])*-1.0);
  }

  return_value = almost_there;

  /*  check for residual roundoff error due to non-representable double's */

  double residual = x - almost_there;

  int* exponent2 =malloc(sizeof(int)*1);
  if (exponent2 == (int*)NULL) {
#ifdef DEBUG_PRINT
    printf("my_trunc_to_sig_dig: malloc failed in my_trunc_to_sig_dig\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return 0.0;
  }

  double mantissa_residual = my_get_mantissa(residual, exponent2);
  if (mantissa_residual < 0.0) {
    mantissa_residual = -mantissa_residual;
  }

  /*  check to see if it needs to be rounded up */

  if (mantissa_residual > 0.5 ) {
    /*  round up */
    if (trunced > 0) {
      trunced = trunced + 1;
      almost_there = (double)(trunced) * pow(10.0, ((int)nsig-exponent[0])*-1.0);
      return_value = almost_there;
    }
    else {
      trunced = trunced - 1;
      almost_there = (double)(trunced) * pow(10.0, ((int)nsig-exponent[0])*-1.0);
      return_value = almost_there;
    }
  }

  /*  check for non-representable float */

  double return_mantissa = my_get_mantissa(return_value, exponent);
  long trunced_mantissa = (long)(return_mantissa * pow(10.0, nsig*1.0));

  /*  if a non-representatble float exists, decrease the number of significant digits and recurse */

  double return_value2;
  if (trunced_mantissa != trunced) {
    return_value2 = my_trunc_to_sig_dig(return_value, nsig-1, returned_sig);
    return_value = return_value2;
    free(exponent);
    exponent = 0;
    free(exponent2);
    exponent2 = 0;
    return return_value;
  }


  /*  return */
 
  free(exponent);
  exponent = 0;
  free(exponent2);
  exponent2 = 0;
  returned_sig[0] = nsig;
 
  return return_value;

}

/***************************************************************/


double  my_flip_bytes(double the_double) {

  /*  flip the byte order -- flips an 8 byte double */

  double return_value = 0.0;

  /********************************************************************/
  /*  error check sizeof double */

  if (sizeof(the_double) != 8) {
#ifdef DEBUG_PRINT
    printf("my_flip_bytes: sizeof double must be 8 -- it is %ld\n", sizeof(the_double));
#endif
    _set_latest_error(SES_DOUBLE_SIZE_ERROR);
    return 0.0;
  }
  /********************************************************************/

  /*  flip the bytes */

  union {
    char the_bytes[8];
    double the_double;
  } myBytes;

   
  myBytes.the_double  = the_double;

  char tmp = myBytes.the_bytes[0];
  myBytes.the_bytes[0] = myBytes.the_bytes[7];
  myBytes.the_bytes[7] = tmp;

  tmp = myBytes.the_bytes[1];
  myBytes.the_bytes[1] = myBytes.the_bytes[6];
  myBytes.the_bytes[6] = tmp;

  tmp = myBytes.the_bytes[2];
  myBytes.the_bytes[2] = myBytes.the_bytes[5];
  myBytes.the_bytes[5] = tmp;

  tmp = myBytes.the_bytes[3];
  myBytes.the_bytes[3] = myBytes.the_bytes[4];
  myBytes.the_bytes[4] = tmp;

  /*  return */

  return_value = myBytes.the_double;

  return return_value;
}
/***************************************************************/

double my_truncate_for_significant_digits(double the_double, unsigned int nsig) {

  /*  trucate the double to a double with only nsig significant digits */

  /*  function prototypes */

  double my_trunc_to_sig_dig(double x, unsigned int nsig, int* returned_sig);

  /*  end function prototypes */

  double return_value = 0.0;

  /********************************************************************/
  int* returned_sig = malloc(sizeof(int)*1);
  if (returned_sig == (int*)NULL) {
#ifdef DEBUG_PRINT
    printf("my_truncate_for_significant_digits: memory allocation error in my_truncate_for_significant_digits");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return 0.0;
  }
  /********************************************************************/

  if (nsig > 0) {
    return_value = my_trunc_to_sig_dig(the_double, nsig, returned_sig); 
  }
  else {
    return_value = the_double;
  }
  free(returned_sig);
  returned_sig = (int*)NULL;



  return return_value;

}
/***************************************************************/

ses_boolean my_do_validation_double(double the_double) {

  /*  check for infinity and Nan */

  ses_boolean return_value = SES_TRUE;

  if (isnan(the_double) != 0) {
    return_value = SES_FALSE;
  }
  if (isinf(the_double) != 0) {
    return_value = SES_FALSE;
  }

  return return_value;
}
/***************************************************************/



/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

/*! \file
 *  \ingroup tests quick
 *  \brief Test improved errorCode checking in eos_ErrorHandler::eos_GetErrorMsg.
 *
 *  See SourceForge issue 
 *  <a href="https://tf.lanl.gov/sf/go/artf6005">artf6005</a>
 *  for details.
 *
 * \note
 * MATIDS TO TEST: none
 */

#include <stdio.h>
#include <stdlib.h>
#include "TEST_FUNCTIONS.h"
#include "eos_Interface.h"

int main ()
{
  EOS_INTEGER errorCode;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  errorCode = EOS_MIN_ERROR_CODE_VALUE - 1;
  eos_GetErrorMessage (&errorCode, errorMessage);
  printf ("%d: %s\n", errorCode, errorMessage);

  errorCode = EOS_MIN_ERROR_CODE_VALUE + 1;
  eos_GetErrorMessage (&errorCode, errorMessage);
  printf ("%d: %s\n", errorCode, errorMessage);

  errorCode = EOS_MAX_ERROR_CODE_VALUE;
  eos_GetErrorMessage (&errorCode, errorMessage);
  printf ("%d: %s\n", errorCode, errorMessage);

  errorCode = EOS_MAX_ERROR_CODE_VALUE + 1;
  eos_GetErrorMessage (&errorCode, errorMessage);
  printf ("%d: %s\n", errorCode, errorMessage);

  return 0;

}

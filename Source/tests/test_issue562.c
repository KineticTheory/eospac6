/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

/*! \file
 *  \ingroup C tests quick
 *  \brief Test the error handling options that are incompatible with EOS_INVERT_AT_SETUP:
 *         EOS_MONOTONIC_IN_X, EOS_MONOTONIC_IN_Y and EOS_USE_MAXWELL_TABLE
 *
 *        See eos/eospac-dev/eospac6#562 for more details:
 *        https://git.lanl.gov/eos/eospac-dev/eospac6/issues/562
 *
 * \note
 * MATIDS TO TEST: 3720
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include "TEST_FUNCTIONS.h"
#include "eos_Interface.h"

int main ()
{
  int i;
  enum
  { nTablesE = 6 };

  EOS_INTEGER tableType[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode;

  EOS_INTEGER nTables;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  typedef struct
  {
      EOS_INTEGER opt1;
      EOS_INTEGER opt2;
  } optionPairs_s;

  optionPairs_s optionPairs[] = {                   /* EXPECTED RESULT */
    { EOS_INVERT_AT_SETUP, EOS_MONOTONIC_IN_X    }, /* FAIL            */ 
    { EOS_INVERT_AT_SETUP, EOS_MONOTONIC_IN_Y    }, /* FAIL            */ 
    { EOS_INVERT_AT_SETUP, EOS_USE_MAXWELL_TABLE }, /* FAIL            */ 
    { EOS_MONOTONIC_IN_X,  EOS_MONOTONIC_IN_Y    }, /* PASS            */ 
    { EOS_MONOTONIC_IN_X,  EOS_USE_MAXWELL_TABLE }, /* PASS            */ 
    { EOS_MONOTONIC_IN_Y,  EOS_USE_MAXWELL_TABLE }  /* PASS            */ 
  };
  
  nTables = nTablesE;

  i = 0;
  if (i < nTables) tableType[i++] = EOS_T_DPt;
  if (i < nTables) tableType[i++] = EOS_T_DPt;
  if (i < nTables) tableType[i++] = EOS_T_DPt;
  if (i < nTables) tableType[i++] = EOS_T_DPt;
  if (i < nTables) tableType[i++] = EOS_T_DPt;
  if (i < nTables) tableType[i++] = EOS_T_DPt;

  assert(i == nTables);

  for (i = 0; i < nTables; i++)
    matID[i] = 3720;

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
  }

  eos_CreateTables (&nTables, tableType, matID, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("%i: eos_CreateTables ERROR %i: %s\n",
                tableHandle[i], tableHandleErrorCode, errorMessage);
      }
    }
    /* report errors, but do not exit */
  }

  /* Enable option pairs */
  for (i = 0; i < nTables; i++) {
    sprintf(errorMessage, "%s", "");
    printf ("--- TEST %i\n", i);
    eos_SetOption (&tableHandle[i], &optionPairs[i].opt1, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: TH=%i %s\n", errorCode, tableHandle[i], errorMessage);
      /* report errors, but do not exit */
    }
    else {
      printf ("eos_SetOption PASS %i: TH=%i %s\n", errorCode, tableHandle[i], errorMessage);
    }
    eos_SetOption (&tableHandle[i], &optionPairs[i].opt2, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: TH=%i %s\n", errorCode, tableHandle[i], errorMessage);
      /* report errors, but do not exit */
    }
    else {
      printf ("eos_SetOption PASS %i: TH=%i %s\n", errorCode, tableHandle[i], errorMessage);
    }
    printf ("\n");
  }

  /* destroy all tables */
  eos_DestroyAll (&errorCode);

  return 0;

}

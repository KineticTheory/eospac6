/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

/*! \file
 * \ingroup tests
 * \brief Verify elimination of _eos_CompressArray platform-dependency
 *        problems. See SourceForge© Issue #artf10445 for more details:
 *        https://tf.lanl.gov/sf/go/artf10445
 *
 * \note
 * MATIDS TO TEST: 9981 9991
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "eos_Interface.h"

int main ()
{
  enum
  { nTablesE = 2 };
  enum
  { nXYPairsE = 4 };

  EOS_INTEGER i, j;
  EOS_REAL X[nXYPairsE], Y[nXYPairsE], F[nXYPairsE], dFx[nXYPairsE],
    dFy[nXYPairsE];
  EOS_INTEGER tableType[nTablesE], numIndVars[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode;
  EOS_INTEGER tableHandleErrorCode;
  EOS_INTEGER nTables;
  EOS_INTEGER nXYPairs;
  EOS_CHAR *tableTypeLabel[nTablesE] = {
    "EOS_T_DPic",
    "EOS_T_DPic"
  };
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  nTables = nTablesE;
  nXYPairs = nXYPairsE;

  tableType[0] = EOS_T_DPic;
  tableType[1] = EOS_T_DPic;

  numIndVars[0] = 2;
  numIndVars[1] = 2;

  matID[0] = 9981;
  matID[1] = 9991;

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
  }

  /*
   * initialize table data objects
   */

  eos_CreateTables (&nTables, tableType, matID, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
      printf ("eos_CreateTables ERROR %i: %s\n", tableHandleErrorCode,
              errorMessage);
    }
  }

  /*
   * set some options
   */
  eos_SetOption (&tableHandle[0], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
  }
  for (i = 1; i < nTables; i++) {
    eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    }
  }

  /*
   * load data into table data objects
   */

  eos_LoadTables (&nTables, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
      printf ("eos_LoadTables ERROR %i (TH=%i): %s\n", tableHandleErrorCode,
              tableHandle[i], errorMessage);
    }
  }

  /*
   * interpolate -- errors codes are intentionally produced
   */
  X[0] = 3000.;
  X[1] = 6000.;
  X[2] = 8200.;
  X[3] = 8300.;

  Y[0] = 20000.0;
  Y[1] = 620000.0;
  Y[2] = 4000000.0;
  Y[3] = 200000000.0;

  for (i = 0; i < nTables; i++) {
    printf ("\n--- Interpolate using tableType %s ---\n", tableTypeLabel[i]);
    eos_Interpolate (&tableHandle[i], &nXYPairs, X, Y, F, dFx, dFy,
                     &errorCode);
    printf ("%s Interpolation Results:\n", tableTypeLabel[i]);
    if (errorCode != EOS_OK) {
      char* p = NULL;
      eos_GetErrorMessage (&errorCode, errorMessage);
      p = strstr(errorMessage, "(err=");
      if (p)
	errorMessage[p-errorMessage] = '#';
      printf ("eos_Interpolate ERROR %i (TH=%i): %s\n", errorCode,
              tableHandle[i], errorMessage);
    }
    else {
      for (j = 0; j < nXYPairs; j++) {
        if (numIndVars[i] == 1)
          printf ("\ti=%i\tX = %e, F = %e, dFx = %e, errorCode: %d\n",
                  j, X[j], F[j], dFx[j], errorCode);
        if (numIndVars[i] == 2)
          printf
            ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e, errorCode: %d\n",
             j, X[j], Y[j], F[j], dFx[j], dFy[j], errorCode);
      }
    }
  }

  /*
   * Destroy all data objects
   */

  eos_DestroyAll (&errorCode);
  if (errorCode != EOS_OK) {
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
      printf ("eos_DestroyAll ERROR %i: %s\n", tableHandleErrorCode,
              errorMessage);
    }
  }

  return 0;

}

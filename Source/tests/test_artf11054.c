/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

/*! \file
 * \ingroup C tests
 *  \brief Verify eos_Interpolate and eos_CheckExtrap discrepencies were fixed.
 *
 *  See SourceForge issue
 *  <a href="https://tf.lanl.gov/sf/go/artf11054">artf11054</a>
 *  for details.
 *
 * \note
 * MATIDS TO TEST: 2700
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"
#include "TEST_FUNCTIONS.h"

int main ()
{
  enum
  { nTablesE = 1 };
  enum
  { nXYPairsE = 4 };

  EOS_INTEGER i, j;
  EOS_REAL X[nXYPairsE], Y[nXYPairsE], F[nXYPairsE], dFx[nXYPairsE],
    dFy[nXYPairsE];
  EOS_INTEGER extrapCode[nXYPairsE];
  EOS_INTEGER tableType[nTablesE], numIndVars[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode;
  EOS_INTEGER tableHandleErrorCode;
  EOS_INTEGER nTables;
  EOS_INTEGER nXYPairs;
  EOS_CHAR *tableTypeLabel[nTablesE] = {
    "EOS_T_DUt"
  };
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  nTables = nTablesE;
  nXYPairs = nXYPairsE;

  tableType[0] = EOS_T_DUt;

  numIndVars[0] = 2;

  matID[0] = 2700;

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

  for (i = 0; i < nTables; i++) {
    EOS_REAL yconv=0.01;
    eos_SetOption (&tableHandle[i], &EOS_Y_CONVERT, &yconv, &errorCode);
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
   * interpolate
   */
  X[0] = 29.;
  X[1] = 29.102883885368;
  X[2] = 30.;
  X[3] = 31.;

  Y[0] = 0.011;
  Y[1] = 0.013;
  Y[2] = 0.015;
  Y[3] = 50.0;

  for (i = 0; i < nTables; i++) {
    printf ("\n--- Interpolate using tableType %s ---\n", tableTypeLabel[i]);
    eos_Interpolate (&tableHandle[i], &nXYPairs, X, Y, F, dFx, dFy,
                     &errorCode);
    printf ("%s Interpolation Results:\n", tableTypeLabel[i]);
    if (errorCode != EOS_OK) {
      EOS_BOOLEAN equal;
      eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &errorCode, &equal);
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_Interpolate ERROR %i (TH=%i): %s\n", errorCode,
              tableHandle[i], errorMessage);
      if (equal) {
	eos_CheckExtrap (&tableHandle[i], &nXYPairs, X, Y, extrapCode,
			 &errorCode);
	if (errorCode != EOS_OK) {
	  printf ("eos_CheckExtrap ERROR %i: %s\n", errorCode,
		  errorMessage);
	}
	else {
	  for (j = 0; j < nXYPairs; j++) {
	    printf
	      ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e, extrap: %i (%s)\n",
	       j, X[j], Y[j], F[j], dFx[j], dFy[j], extrapCode[j], ERROR_TO_TEXT(extrapCode[j]));
	  }
	}
      }
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

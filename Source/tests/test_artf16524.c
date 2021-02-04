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
 *  \brief Ensure eos_Interpolate and eos_CheckExtrap generate and report expected extrapolation
 *         error codes.
 *
 *  See SourceForge issue
 *  <a href="https://tf.lanl.gov/sf/go/artf16524">artf16524</a>
 *  for details.
 *
 * \note
 * MATIDS TO TEST: 3720
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"
#include "TEST_FUNCTIONS.h"

int main ()
{
  enum
  { nTablesE = 2 };
  enum
  { nXYPairsE = 16 };

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
    "EOS_Ut_DT",
    "EOS_T_DUt"
  };
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  nTables = nTablesE;
  nXYPairs = nXYPairsE;

  i=0;
  tableType[i++] = EOS_Ut_DT;
  tableType[i++] = EOS_T_DUt;

  i=0;
  numIndVars[i++] = 2;
  numIndVars[i++] = 2;

  i=0;
  matID[i++] = 3720;
  matID[i++] = 3720;

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
   * Interpolate and extrapolate given the following tabulated data ranges for matid 3720:
   *   0 <= X <= 54000      g/cc
   *   0 <= Y <= 1160485000 K
   */
  X[0] = -10;     Y[0] = -10;
  X[1] = -10;     Y[1] = 0;
  X[2] = -10;     Y[2] = 1160485000;
  X[3] = -10;     Y[3] = 1276533500;
  X[4] = 0;       Y[4] = -10;
  X[5] = 0;       Y[5] = 0;
  X[6] = 0;       Y[6] = 1160485000;
  X[7] = 0;       Y[7] = 1276533500;
  X[8] = 54000;   Y[8] = -10;
  X[9] = 54000;   Y[9] = 0;
  X[10] = 54000;  Y[10] = 1160485000;
  X[11] = 54000;  Y[11] = 1276533500;
  X[12] = 59400;  Y[12] = -10;
  X[13] = 59400;  Y[13] = 0;
  X[14] = 59400;  Y[14] = 1160485000;
  X[15] = 59400;  Y[15] = 1276533500;

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
	eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &errorCode, &equal);
	if (! equal && errorCode != EOS_OK) {
	  printf ("eos_CheckExtrap ERROR %i: %s\n", errorCode,
		  errorMessage);
	}
	else {
	  for (j = 0; j < nXYPairs; j++) {
	    printf
	      ("\ti=%i\tX = %13.6e, Y = %13.6e, F = %13.6e, dFx = %13.6e, dFy = %13.6e, extrap: %5d (%s)\n",
	       j, X[j], Y[j], F[j], dFx[j], dFy[j], extrapCode[j], ERROR_TO_TEXT(extrapCode[j]));
	  }
	}
      }
    }
    else {
      for (j = 0; j < nXYPairs; j++) {
        if (numIndVars[i] == 1)
          printf ("\ti=%i\tX = %13.6e, F = %13.6e, dFx = %13.6e, errorCode: %d\n",
                  j, X[j], F[j], dFx[j], errorCode);
        if (numIndVars[i] == 2)
          printf
            ("\ti=%i\tX = %13.6e, Y = %13.6e, F = %13.6e, dFx = %13.6e, dFy = %13.6e, errorCode: %d\n",
             j, X[j], Y[j], F[j], dFx[j], dFy[j], errorCode);
      }
    }

    printf("\n");

    for (j = 0; j < nXYPairs; j++) {
      Y[j] = F[j];
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

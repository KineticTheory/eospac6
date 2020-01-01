/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

/*! \file
 *  \ingroup tests
 *  \brief Perform the following tests:
 *    -# Verify memory error in eos_CleanUpColdCurveRecordType1 was fixed
 *
 *  See SourceForge issues
 *  <a href="https://tf.lanl.gov/sf/go/artf3926">artf3926</a>
 *  and
 *  <a href="https://tf.lanl.gov/sf/go/artf3939">artf3939</a>
 *  for details.
 *
 * \note
 * MATIDS TO TEST: 5250
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"

int main ()
{
  int i;
  EOS_REAL *X, *Y, *FXY, *dFx, *dFy;
  EOS_INTEGER xyBounds[4];
  enum
  { nTablesE = 3 };
  enum
  { nXYPairsE = 4 };
  //    enum {nTablesE = 9};

  EOS_INTEGER tableType[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode;

  EOS_INTEGER nTables, nXYPairs;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  nTables = nTablesE;
  nXYPairs = nXYPairsE;

  tableType[0] = EOS_Pc_D;
  tableType[1] = EOS_Uc_D;
  tableType[2] = EOS_Ac_D;

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
    matID[i] = 5250;
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
    return 0;
  }

  eos_SetOption (&tableHandle[0], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    return 0;
  }

  for (i = 1; i < nTables; i++) {
    eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr,
                   &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      return 0;
    }
  }

  eos_LoadTables (&nTables, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("%i: eos_LoadTables ERROR %i: %s\n",
                tableHandle[i], tableHandleErrorCode, errorMessage);
      }
    }
    return 0;
  }

  /* testing interpolation methods */

  /* allocate memory continuously */
  X = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  Y = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  FXY = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  dFx = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  dFy = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);

  X[0] = 1.3e-06;
  X[1] = 1.0e-04;
  X[2] = 1.0e-02;
  X[3] = 0.1;

  Y[0] = 13.8;
  Y[1] = 14.0;
  Y[2] = 0.0;
  Y[3] = 0.0;

  printf ("\n--- TEST eos_Interpolate using category 0 tableType: EOS_Pc_D ---\n");
  errorCode = EOS_OK;
  eos_Interpolate (&tableHandle[0], &nXYPairs, X, Y, FXY, dFx, dFy, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
  }
  eos_CheckExtrap (&tableHandle[0], &nXYPairs, X, Y, xyBounds, &errorCode);
  for (i = 0; i < nXYPairsE; i++) {
    printf
      ("\ti=%i\tX = %23.15e, Y = %23.15e, F = %23.15e, dFx = %23.15e, xyBounds: %d\n",
       i, X[i], Y[i], FXY[i], dFx[i], xyBounds[i]);
  }

  printf ("\n--- TEST eos_Interpolate using category 0 tableType: EOS_Uc_D ---\n");
  errorCode = EOS_OK;
  eos_Interpolate (&tableHandle[1], &nXYPairs, X, Y, FXY, dFx, dFy, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
  }
  eos_CheckExtrap (&tableHandle[1], &nXYPairs, X, Y, xyBounds, &errorCode);
  for (i = 0; i < nXYPairsE; i++) {
    printf
      ("\ti=%i\tX = %23.15e, Y = %23.15e, F = %23.15e, dFx = %23.15e, xyBounds: %d\n",
       i, X[i], Y[i], FXY[i], dFx[i], xyBounds[i]);
  }

  printf ("\n--- TEST eos_Interpolate using category 0 tableType: EOS_Ac_D ---\n");
  errorCode = EOS_OK;
  eos_Interpolate (&tableHandle[2], &nXYPairs, X, Y, FXY, dFx, dFy, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
  }
  eos_CheckExtrap (&tableHandle[2], &nXYPairs, X, Y, xyBounds, &errorCode);
  for (i = 0; i < nXYPairsE; i++) {
    printf
      ("\ti=%i\tX = %23.15e, Y = %23.15e, F = %23.15e, dFx = %23.15e, xyBounds: %d\n",
       i, X[i], Y[i], FXY[i], dFx[i], xyBounds[i]);
  }

  eos_DestroyAll (&errorCode);

  free(X);
  free(Y);
  free(FXY);
  free(dFx);
  free(dFy);

  return 0;

}
